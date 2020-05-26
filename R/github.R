
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
                      "Sys.setenv() to set a github graphQL tokan ",
                      "named 'GITHUB', 'GITHUBQL', or similar"))
    return (unique (toks))
}

get_qry <- function (gh_cli, org, repo, endCursor = NULL, branch = "master") {
    q <- paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
                   collaborators {
                       nodes {
                           name
                           login
                       }
                   }
    }
    }")
    qry <- ghql::Query$new()
    qry$query('get_commits', q)

    return (qry)
}

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
    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )


    qry <- get_qry (gh_cli, org = org, repo = repo)
    x <- gh_cli$exec(qry$queries$get_commits) %>%
        jsonlite::fromJSON ()

    res <- x$data$repository$collaborators$nodes

    if (alphabetical)
        res <- res [order (res$login), ]

    return (res)
}

