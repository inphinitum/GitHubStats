# GitHubStats
This project is meant to provide insights about speed of iteration for software development teams that use GitHub.

The intention is to provide teams with insights about how their development process actually works, and how to improve via insights (e.g. PRs take a long time to get merged).

### Data collection
These scripts use GitHub's v4 GraphQL API to gather data for the desired organization.

### Current status
It's very much work in progress. At the moment 

## Usage
The following environment variables are needed:

- GITHUB_PAT - GitHub Personal Authentication Token
- GITHUB_ORG - Organization in GitHub that you want to collect stats for

To get a data frame with all PRs for your organization, run:

```
> client <- getGHConnection()
> pullRequestDf <- getPullRequests(client)
```