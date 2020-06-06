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
  con$load_schema()
  
  if(con$ping())
    con
  else
    NULL
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
  
  qry
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
  repositoryDf  <- data.frame(rsp$data$organization$repositories$nodes$name)
  
  # Keep collecting repos in following pages, and append results
  while(moreRepos) {
    variables$after <- repoEndCursor
    rsp <- client$exec(queries$queries$reposNextPage, variables)
    rsp <- jsonlite::fromJSON(rsp)
    
    moreRepos     <- rsp$data$organization$repositories$pageInfo$hasNextPage
    repoEndCursor <- rsp$data$organization$repositories$pageInfo$endCursor
    repositoryDf  <- rbind(repositoryDf, data.frame(rsp$data$organization$repositories$nodes$name))
  }
  
  colnames(repositoryDf) <- c("repoName")
  repositoryDf
}

## getPullRequests(GraphQLClient)
##
## Input:
## - client: GraphQLClient object, already initialized
##
## Output:
## - data.frame with following variables:
##   - pullRequestId: Unique identifier of the pull request
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
  
  print(paste("Gathering repositories for", organization, "..."))
  repositoriesDf <- getRepositories(client)
  
  # Gather all graphql queries, separated for clarity
  queries <- createQueries()
  
  for(repo in repositoriesDf$repoName) {
    cat(paste("Processing repository", repo))
    
    variables <- list(repo = repo, login = organization, pageSize = pageSize)
    rsp <- client$exec(queries$queries$pullRequestsInRepoFirstPage, variables)
    rsp <- jsonlite::fromJSON(rsp)
    
    morePullRequests <- rsp$data$repository$pullRequests$pageInfo$hasNextPage
    endCursor        <- rsp$data$repository$pullRequests$pageInfo$endCursor
    pullRequestDf    <- rbind(pullRequestDf,
                              data.frame(rsp$data$repository$pullRequests$nodes$id,
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
                                           rsp$data$repository$pullRequests$nodes$createdAt,
                                           rsp$data$repository$pullRequests$nodes$closedAt,
                                           rsp$data$repository$pullRequests$nodes$author$login,
                                           rsp$data$repository$pullRequests$nodes$repository$name))
    }
    cat(" Done!\n")
  }
  
  colnames(pullRequestDf) <- c('pullRequestId', 'createdAt', 'closedAt', 'author', 'repoName')
  pullRequestDf
}

writeDataToDisk <- function(dataframe, filename) {
  path <- file.path(getwd(), "data")
  if(!dir.exists(path))
    dir.create(path)
  
  filename <- paste(filename, "-", Sys.time(), ".csv", sep = "")
  
  write.csv(dataframe, file.path(path, filename))
}
