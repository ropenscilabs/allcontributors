#' get_contributors
#'
#' Get all contributors to a repository, including those who contribute to code,
#' open issues, and contribute to discussions in issues.
#' @param org Github organisation name for repository
#' @param repo Repository within `org` for which contributors are to be
#' extracted
#' @param exclude_issues Numbers of any issues (or pull requests) to be excluded
#' from lists of contributors.
#' @param quiet If `FALSE`, display progress information on screen.
#' @inheritParams add_contributors
#'
#' @examples
#' \dontrun{
#' get_contributors (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @export
get_contributors <- function (org, repo,
                              type = c ("code", "issues", "discussion"),
                              exclude_issues = NULL,
                              alphabetical = FALSE,
                              quiet = FALSE) {

    if (!quiet) {
        cat (cli::col_cyan (cli::symbol$star), " Extracting code contributors")
        utils::flush.console ()
    }

    ctb_code <- get_gh_code_contributors (org,
                                          repo,
                                          alphabetical = alphabetical)
    ctb_code <- ctb_code [which (!is.na (ctb_code$login)), ]
    ctb_code$type <- "code"
    if (!quiet)
        message ("\r", cli::col_green (cli::symbol$tick),
                 " Extracted code contributors   ")

    issue_authors <- issue_contributors <- NULL
    if ("issues" %in% type) {
        if (!quiet) {
            cat (cli::col_cyan (cli::symbol$star),
                 " Extracting github issue contributors")
            utils::flush.console ()
        }
        ctb_issues <- get_gh_issue_people (org = org, repo = repo,
                                           exclude_issues = exclude_issues)

        index <- which (!ctb_issues$authors$login %in% ctb_code$logins)
        ctb_issues$authors <- ctb_issues$authors [index, ]

        index <- which (!ctb_issues$contributors$login %in%
                        c (ctb_code$logins, ctb_issues$authors$login))
        ctb_issues$contributors <- ctb_issues$contributors [index, ]

        add_na_contribs <- function (x, type) {
            x <- cbind (x, NA_integer_) [, c (1, 3, 2)]
            names (x) [2] <- "contributions"
            x$type <- type
            return (x)
        }
        if (nrow (ctb_issues$authors) > 0)
            issue_authors <- add_na_contribs (ctb_issues$authors,
                                              "issue_authors")
        if ("discussion" %in% type & nrow (ctb_issues$contributors) > 0)
            issue_contributors <- add_na_contribs (ctb_issues$contributors,
                                                   "issue_contributors")

        if (!quiet)
            message ("\r", cli::col_green (cli::symbol$tick),
                     " Extracted github issue contributors    ")
    }

    ctbs <- rbind (ctb_code, issue_authors, issue_contributors)
    rownames (ctbs) <- NULL

    return (ctbs)
}

#' get_gh_code_contributors
#'
#' Get list of all code contributors to the code of a repository
#' @inheritParams get_contributors
#' @return A `data.frame` of two columns of contributor (name, login)
#'
#' @examples
#' \dontrun{
#' get_gh_code_contributors (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @export
get_gh_code_contributors <- function (org, repo, alphabetical = FALSE) {

    tok <- get_gh_token ()
    if (length (tok) == 0)
        tok <- ""
    user <- get_git_user ()

    # This query can not be done via GraphQL, so have to use v3 REST API
    u <- paste0 ("https://api.github.com/repos/",
                 org,
                 "/",
                 repo,
                 "/contributors")

    if (nchar (tok) > 0) {
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
        #u2 <- paste0 (u, "?page=", pg)
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
        toks <- e [grep ("GITHUB|GH", names (e))]
        if (length (toks) > 1) {
            un <- unique (toks)
            names (un) <- names (toks) [match (un, toks)]
            is_a_token <- which (!grepl ("^http(s?)://", un))
            un <- un [is_a_token]
            if (length (un) > 1)
                un <- un [grep ("TOKEN", names (un))]
            if (length (un) > 1)
                un <- un [grep ("QL", names (un))]
            toks <- un
        }
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


get_issues_qry <- function (gh_cli, org, repo, end_cursor = NULL) {
    after_txt <- ""
    if (!is.null (end_cursor))
        after_txt <- paste0 (", after:\"", end_cursor, "\"")

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
                                   number
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
#'
#' @examples
#' \dontrun{
#' get_gh_issue_people (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @export
get_gh_issue_people <- function (org, repo, exclude_issues = NULL) {

    token <- get_gh_token ()
    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    has_next_page <- TRUE
    end_cursor <- NULL
    issue_authors <- NULL
    issue_contributors <- issue_contributors_avatar <- list ()
    while (has_next_page) {
        qry <- ghql::Query$new()
        q <- get_issues_qry (gh_cli, org = org, repo = repo,
                             end_cursor = end_cursor)
        qry$query('issues', q)

        dat <- gh_cli$exec(qry$queries$issues) %>%
            jsonlite::fromJSON ()

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges
        issue_authors <- c (issue_authors, dat$node$author$login)
        #issue_avatars <- c (issue_author_avatar, dat$node$author$avatarUrl)

        author <- dat$node$participants$edges

        author_login <- lapply (author, function (i) i$node$login)
        author_avatar <- lapply (author, function (i) i$node$avatarUrl)
        issue_contributors <- c (issue_contributors, author_login)
        issue_contributors_avatar <- c (issue_contributors_avatar,
                                        author_avatar)
    }

    if (!is.null (exclude_issues)) {

        if (any (!exclude_issues %in% seq (issue_authors)))
            stop ("exclude_issues extends beyond range of issues in this repo")

        issue_authors <- issue_authors [-exclude_issues]
        author_login <- author_login [-exclude_issues]
        author_avatar <- author_avatar [-exclude_issues]

        issue_contributors <- issue_contributors [-exclude_issues]
        issue_contributors_avatar <- issue_contributors_avatar [-exclude_issues]
    }

    author_login <- unlist (author_login)
    author_avatar <- unlist (author_avatar)
    index <- which (!duplicated (author_login))
    author_login <- author_login [index]
    author_avatar <- author_avatar [index]

    issue_contributors <- unlist (issue_contributors)
    issue_contributors_avatar <- unlist (issue_contributors_avatar)
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


#' get_gh_issue_titles
#'
#' Extract titles and numbers of all issues associated with a nominated
#' repository
#'
#' @inheritParams get_contributors
#' @return `data.frame` with one column of issue numbers, and one column of
#' issue titles.
#'
#' @examples
#' \dontrun{
#' get_gh_issue_titles (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @export
get_gh_issue_titles <- function (org, repo) {

    token <- get_gh_token ()
    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    has_next_page <- TRUE
    end_cursor <- NULL
    issue_title <- issue_number <- NULL
    while (has_next_page) {
        qry <- ghql::Query$new()
        q <- get_issues_qry (gh_cli, org = org, repo = repo,
                             end_cursor = end_cursor)
        qry$query('issues', q)

        dat <- gh_cli$exec(qry$queries$issues) %>%
            jsonlite::fromJSON ()

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges
        issue_title <- c (issue_title, dat$node$title)
        issue_number <- c (issue_number, dat$node$number)
    }

    data.frame (number = issue_number,
                title = issue_title,
                stringsAsFactors = FALSE)
}

#' get_gh_contrib_issue
#'
#' Extract contributors currently listed on an "All Contributions" issue in a
#' github repository.
#'
#' @inheritParams get_contributors
#' @return Character vector of github logins for all contributors listed in
#' current issue, or empty character string if there no issue named "All
#' Contributors".
#'
#' @examples
#' \dontrun{
#' get_gh_contrib_issue (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @export
get_gh_contrib_issue <- function (org, repo) {

    # Note that this is much easier with the REST API than via graphql.

    issues <- get_gh_issue_titles (org, repo)
    issue_num <- issues$number [grep ("all contrib", tolower (issues$title))]
    if (length (issue_num) == 0)
        return (NULL)

    tok <- get_gh_token ()
    user <- get_git_user ()

    u <- paste0 ("https://api.github.com/repos/",
                 org,
                 "/",
                 repo,
                 "/issues/",
                 issue_num)

    qry <- list (state = "all", per_page = 100, page = 1)
    if (nchar (tok) > 0) {
        x <- httr::GET (u, httr::authenticate (user, tok), query = qry)
    } else {
        x <- httr::GET (u, query = qry)
    }

    txt <- httr::content (x)$body
    # That's just the body of the opening comment; the following lines extract
    # all subsequent comments:
    if (nchar (tok) > 0) {
        x <- httr::GET (paste0 (u, "/comments"),
                        httr::authenticate (user, tok),
                        query = qry)
    } else {
        x <- httr::GET (paste0 (u, "/comments"), query = qry)
    }
    txt <- c (txt, lapply (httr::content (x), function (i) i$body))

    pings <- lapply (txt, function (i) {
        first <- regexpr ("@", i)
        i <- strsplit (substring (i, first - 1, nchar (i)), "\\r") [[1]]
        gsub ("\\n", "", i [grep ("@", i)])
                    })

    unlist (pings)
}
