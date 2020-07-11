## getGHConnection()
##
## Input:
##
## Output:
##  - GraphqlClient initialized to interact with GitHub's GraphQL API, or NULL if unsuccessful.
##
## Uses authentication token defined in GITHUB_PAT environment variable.
##
getGHConnection <- function() {
  library(ghql) 
  
  # Initialize GraphQl connection to GH with personal token
  token <- Sys.getenv("GITHUB_PAT")
  con <- GraphqlClient$new(
    url <- "https://api.github.com/graphql",
    headers = list(Authorization = paste0("Bearer ", token))
  )
  
  if(con$ping()) {
    cat("Connection test OK!\n")
    cat("Reading GitHub API schema...\n")
    
    con$load_schema()
    con
  }
  else {
    cat("ERROR: Connection test failed!\n")
    NULL
  }
}

## createqueries()
##
## Input:
##
## Output:
## - ghql::Query with the following queries
##   - reposFirstPage
##   - reposNext Page
##   - pullRequestFirstPage
##   - pullRequestsNextPage
##
createQueries <- function() {
  # Prepare the queries we're going to use: one to gather the first page, another one for following pages
  qry <- Query$new()
  qry$query('reposFirstPage',
            'query reposFirstPage($login: String!, $pageSize: Int!) {
    organization(login:$login) {
      repositories(orderBy: {field:PUSHED_AT,direction:DESC}, isLocked:false, first:$pageSize) {
        nodes {
          name
          isArchived
          isDisabled
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }')
  
  qry$query('reposNextPage',
            'query reposNextPage($login: String!, $pageSize: Int!, $after: String!) {
    organization(login:$login) {
      repositories(orderBy: {field:PUSHED_AT,direction:DESC}, isLocked:false, first:$pageSize, after:$after) {
        nodes {
          name
          isArchived
          isDisabled
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }')
  
  qry$query('pullRequestsInRepoFirstPage',
            'query pullRequestsInRepoFirstPage($repo: String!, $login: String!, $pageSize: Int!) {
    repository(name:$repo, owner:$login) {
      pullRequests(first:$pageSize) {
        nodes {
          id
          state
          createdAt
          closedAt
          author {
            login
          }
          repository {
            name
          }
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }')
  
  qry$query('pullRequestsInRepoNextPage',
            'query pullRequestsInRepoNextPage($repo: String!, $login: String!, $pageSize: Int!, $after: String!) {
    repository(name:$repo, owner:$login) {
      pullRequests(first:$pageSize, after:$after) {
        nodes {
          id
          state
          createdAt
          closedAt
          author {
            login
          }
          repository {
            name
          }
        }
        pageInfo {
          hasNextPage
          endCursor
        }
      }
    }
  }')
  
  qry$query('teams',
            'query teams($login: String!, $pageSize: Int!) {
               organization(login:$login) {
                 teams (first:$pageSize) {
                   nodes {
                     name
                   }
                   pageInfo {
                     hasNextPage
                     endCursor
                   }
                 }
               }
            }')
  
  qry
}

getTeams <- function(client) {
  organization <- Sys.getenv("GITHUB_ORG")
  pageSize     <- 100
  
  # Get all GraphQL queries. Separated for simplicity.
  queries <- createQueries()
  
  variables <- list(login = organization, pageSize = pageSize)
  rsp <- client$exec(queries$queries$teams, variables)
  rsp <- jsonlite::fromJSON(rsp)
}

## getRepositories(GraphqlClient client)
##
## Input:
## - client: GraphqlClient object, already initialized
##
## Output:
## - Dataframe with one variable, repoName
##
## Returns all repositories for the organization defined in the environment variable GITHUB_ORG.
##
getRepositories <- function(client) {
  organization <- Sys.getenv("GITHUB_ORG")
  pageSize     <- 100
  repositoryDf <- NULL
  
  # Get all GraphQL queries. Separated for simplicity.
  queries <- createQueries()
  
  # Execute the query that returns the first page of repositories
  variables <- list(login = organization, pageSize = pageSize)
  rsp <- client$exec(queries$queries$reposFirstPage, variables)
  rsp <- jsonlite::fromJSON(rsp)
  
  # Gather pagination data to see if we need to keep collecting repo info, and content of first page
  moreRepos     <- rsp$data$organization$repositories$pageInfo$hasNextPage
  repoEndCursor <- rsp$data$organization$repositories$pageInfo$endCursor
  repositoryDf  <- data.frame(rsp$data$organization$repositories$nodes$name,
                              rsp$data$organization$repositories$nodes$isArchived,
                              rsp$data$organization$repositories$nodes$isDisabled)
  
  # Keep collecting repos in following pages, and append results
  while(moreRepos) {
    variables$after <- repoEndCursor
    rsp <- client$exec(queries$queries$reposNextPage, variables)
    rsp <- jsonlite::fromJSON(rsp)
    
    moreRepos     <- rsp$data$organization$repositories$pageInfo$hasNextPage
    repoEndCursor <- rsp$data$organization$repositories$pageInfo$endCursor
    repositoryDf  <- rbind(repositoryDf, data.frame(rsp$data$organization$repositories$nodes$name,
                                                    rsp$data$organization$repositories$nodes$isArchived,
                                                    rsp$data$organization$repositories$nodes$isDisabled))
  }
  
  
  colnames(repositoryDf) <- c("repoName", "isArchived", "isDisabled")
  repositoryDf %>%
    filter(isArchived == FALSE, isDisabled == FALSE) %>%
    select(c(repoName))
}

## getPullRequests(GraphQLClient)
##
## Input:
## - client: GraphQLClient object, already initialized
##
## Output:
## - data.frame with following variables:
##   - pullRequestId: Unique identifier of the pull request
##   - state:         State of the PR: CLOSED, MERGED, OPEN
##   - createdAt:     Date of creation
##   - closedAt:      Date of closure
##   - author:        GitHub login of the pull request author
##   - repoName:      Name of the repository where the pull request was created
##
## Returns all pull requests in the organization.
##
getPullRequests <- function(client) {
  
  organization  <- Sys.getenv("GITHUB_ORG")
  pageSize      <- 100
  pullRequestDf <- data.frame()
  
  cat("Gathering repositories for '", organization, "'...\n", sep = "")
  repositoriesDf <- getRepositories(client)
  
  # Gather all graphql queries, separated for clarity
  queries <- createQueries()
  
  for(repo in repositoriesDf$repoName) {
    cat("Processing repository '", repo, "'", sep = "")
    
    variables <- list(repo = repo, login = organization, pageSize = pageSize)
    rsp <- client$exec(queries$queries$pullRequestsInRepoFirstPage, variables)
    rsp <- jsonlite::fromJSON(rsp)
    
    morePullRequests <- rsp$data$repository$pullRequests$pageInfo$hasNextPage
    endCursor        <- rsp$data$repository$pullRequests$pageInfo$endCursor
    pullRequestDf    <- rbind(pullRequestDf,
                              data.frame(rsp$data$repository$pullRequests$nodes$id,
                                         rsp$data$repository$pullRequests$nodes$state,
                                         rsp$data$repository$pullRequests$nodes$createdAt,
                                         rsp$data$repository$pullRequests$nodes$closedAt,
                                         rsp$data$repository$pullRequests$nodes$author$login,
                                         rsp$data$repository$pullRequests$nodes$repository$name))
    
    while(morePullRequests) {
      cat(".")
      variables$after <- endCursor
      rsp <- client$exec(queries$queries$pullRequestsInRepoNextPage, variables)
      rsp <- jsonlite::fromJSON(rsp)
      
      morePullRequests <- rsp$data$repository$pullRequests$pageInfo$hasNextPage
      endCursor        <- rsp$data$repository$pullRequests$pageInfo$endCursor
      pullRequestDf    <- rbind(pullRequestDf,
                                data.frame(rsp$data$repository$pullRequests$nodes$id,
                                           rsp$data$repository$pullRequests$nodes$state,
                                           rsp$data$repository$pullRequests$nodes$createdAt,
                                           rsp$data$repository$pullRequests$nodes$closedAt,
                                           rsp$data$repository$pullRequests$nodes$author$login,
                                           rsp$data$repository$pullRequests$nodes$repository$name))
    }
    cat(" Done!\n")
  }
  
  colnames(pullRequestDf) <- c('pullRequestId', 'state', 'createdAt', 'closedAt', 'author', 'repoName')
  pullRequestDf
}

## writeDataToDisk(dataframe, filename)
##
## Input:
## - dataframe: data.frame to be written to disk
## - filename: base name for the file
##
## Output:
##
## Writes the given dataframe to disk in csv format taking the base filename and adding the date and time.
##
writeDataToDisk <- function(dataframe, filename) {
  path <- file.path(getwd(), "data")
  if(!dir.exists(path))
    dir.create(path)
  
  filename <- paste(filename, "-", Sys.time(), ".csv", sep = "")
  
  write.csv(dataframe, file.path(path, filename))
}

## writePullRequestData(dataframe, filename)
##
## Input:
## - Dataframe with pull request info
##
## Output:
##
## Writes pullRequestDf to disk, creating the "data" folder in the working directory if necessary.
## It creates a file with the following format: pullRequests-DATE TIME.csv
##
writePullRequestData <- function(pullRequestDf) {
  writeDataToDisk(pullRequestDf, "pullRequests")
}

## readLatestPullRequestData()
##
## Input:
## - forceAPIUpdate Boolean, whether to force reading from GitHub even if data exists for the current day.
##
## Output:
## - data.frame with the read data if it exists, NULL otherwise.
##
## Reads the latest pull request data from the "data" directory  if it's up to date, otherwise it reads
## directly from GitHub, and returns it in a data.frame.
##
readLatestPullRequestData <- function(forceAPIUpdate = FALSE) {
  pullRequestDf <- NULL
  
  startTime <- Sys.time()
  path <- file.path(getwd(), "data")
  if(dir.exists(path)) {
    files <- sort(list.files(path, "pullRequests.*[.]csv"), decreasing = TRUE)
    
    if(!forceAPIUpdate & length(files) > 0 & date(file.info(file.path(path, files[1]))$ctime) == Sys.Date()) {
      cat(paste("Cached is up to date, reading pull request data from", file.path(path, files[1]), "\n"))
      pullRequestDf <- read.csv(file.path(path, files[1]))
      pullRequestDf <- pullRequestDf[, 2:7]
    }
    else {
      cat("Cached data is outdated, reading pull request data from GitHub\n")
      client <- getGHConnection()
      pullRequestDf <- getPullRequests(client)
      writePullRequestData(pullRequestDf)
    }
  }
  else {
    cat("No cached data available, reading pull request data from GitHub\n")
    client <- getGHConnection()
    pullRequestDf <- getPullRequests(client)
    writePullRequestData(pullRequestDf)
  }
  
  endTime <- Sys.time()
  cat("Gathering pull request information took", endTime - startTime, "\n")
  pullRequestDf
}
