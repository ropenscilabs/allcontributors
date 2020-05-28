
#' get_contributors
#'
#' Get list of all contributors to a repository
#' @param org Github organisation name for repository
#' @param repo Repository within `org` for which contributors are to be
#' extracted
#' @param alphabetical If `TRUE` contributors are alphabetically sorted by
#' login.
#' @return A `data.frame` of two columns of contributor (name, login)
#' @export
get_contributors <- function (org, repo, alphabetical = FALSE) {

    tok <- get_gh_token ()
    user <- get_git_user ()

    # This query can not be done via GraphQL, so have to use v3 REST API
    u <- paste0 ("https://api.github.com/repos/",
                 org,
                 "/",
                 repo,
                 "/contributors")

    if (length (tok) > 0) {
        x <- httr::GET (u, httr::authenticate (user, tok)) %>%
            httr::content ()
    } else {
        x <- httr::GET (u) %>%
            httr::content ()
    }
    pg <- 1
    res <- x
    while (length (x) == 30) {
        pg <- pg + 1
        u2 <- paste0 (u, "?page=", pg)
        if (length (tok) > 0) {
            x <- httr::GET (u, httr::authenticate (user, tok)) %>%
                httr::content ()
        } else {
            x <- httr::GET (u) %>%
                httr::content ()
        }
        res <- c (res, x)
    }

    logins <- vapply (x, function (i) i$login, character (1))
    contributions <- vapply (x, function (i) i$contributions, integer (1))
    avatars <- vapply (x, function (i) i$avatar_url, character (1))

    index <- seq (logins)
    if (alphabetical)
        index <- order (res$logins)

    data.frame (logins = logins,
                contributions = contributions,
                avatar = avatars,
                stringsAsFactors = FALSE) [index, ]
}

get_gh_token <- function (token = "") {
    e <- Sys.getenv ()
    if (token != "")
        toks <- e [grep (token, names (e))]
    else {
        toks <- e [grep ("GITHUB", names (e))]
        if (length (toks) > 1)
            toks <- toks [grep ("QL", names (toks))]
    }

    if (length (unique (toks)) > 1)
        stop (paste0 ("No unambiguous token found; please use ",
                      "Sys.setenv() to set a github tokan with a ",
                      "name which includes 'GITHUB'"))
    return (unique (toks))
}

get_git_user <- function () {
    #whoami::whoami ()
    git2r::config ()$global$user.name
}


# ********************  GITHUB ISSUE EXTRACTION ********************  


get_issues_qry <- function (gh_cli, org, repo, endCursor = NULL) {
    after_txt <- ""
    if (!is.null (endCursor))
        after_txt <- paste0 (", after:\"", endCursor, "\"")

    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   issues (first: 100", after_txt, ") {
                       pageInfo {
                           hasNextPage
                           endCursor
                       }
                       edges {
                           node {
                               ... on Issue {
                                   createdAt
                                   author {
                                       login
                                       avatarUrl
                                   }
                                   title
                                   url,
                                   participants (first: 100) {
                                       pageInfo {
                                           hasNextPage
                                           endCursor
                                       }
                                       edges {
                                           node {
                                               login
                                               avatarUrl
                                           }
                                       }
                                   }
                               }
                           }
                       }
                   }
                }
        }")

    return (q)
}

#' get_gh_issue_people
#'
#' Extract lists of (1) all authors of, and (2) all contributors to, all github
#' issues for nominated repository
#'
#' @inheritParams get_contributors
#' @return List of (authors, contributors), each as character vector of github
#' login names.
#' @export
get_gh_issue_people <- function (org, repo) {

    token <- get_gh_token ()
    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    hasNextPage <- TRUE
    endCursor <- NULL
    issue_authors <- issue_author_avatar <-
        issue_contributors <- issue_contributors_avatar <- NULL
    while (hasNextPage) {
        qry <- ghql::Query$new()
        q <- get_issues_qry (gh_cli, org = org, repo = repo,
                             endCursor = endCursor)
        qry$query('issues', q)

        dat <- gh_cli$exec(qry$queries$issues) %>%
            jsonlite::fromJSON ()

        hasNextPage <- dat$data$repository$issues$pageInfo$hasNextPage
        endCursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges
        issue_authors <- c (issue_authors, dat$node$author$login)
        issue_avatars <- c (issue_author_avatar, dat$node$author$avatarUrl)

        author <- dat$node$participants$edges

        author_login <- unlist (lapply (author, function (i) i$node$login))
        author_avatar <- unlist (lapply (author, function (i) i$node$avatarUrl))
        issue_contributors <- c (issue_contributors, author_login)
        issue_contributors_avatar <- c (issue_contributors_avatar, author_avatar)
    }

    index <- which (!duplicated (author_login))
    author_login <- author_login [index]
    author_avatar <- author_avatar [index]

    index <- which (!(duplicated (issue_contributors) |
                      issue_contributors %in% author_login))
    issue_contributors <- issue_contributors [index]
    issue_contributors_avatar <- issue_contributors_avatar [index]

    list (authors = data.frame (logins = author_login,
                                avatar = author_avatar,
                                stringsAsFactors = FALSE),
          contributors = data.frame (logins = issue_contributors,
                                     avatar = issue_contributors_avatar,
                                     stringsAsFactors = FALSE))
}

