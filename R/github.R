#' get_contributors
#'
#' Get all contributors to a repository, including those who contribute to code,
#' open issues, and contribute to discussions in issues.
#' @param org Github organisation name for repository
#' @param repo Repository within `org` for which contributors are to be
#' extracted
#' @param check_urls If `TRUE` (default), GitHub URLs of all contributors are
#' checked to ensure they are still valid. (This is generally the most
#' time-consuming stage, so set to 'FALSE' if you are sure all URLs are valid.)
#' @param quiet If `FALSE`, display progress information on screen.
#' @inheritParams add_contributors
#'
#' @examples
#' \dontrun{
#' get_contributors (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @family main
#' @export
get_contributors <- function (org, repo,
                              type = c ("code", "issues", "discussion"),
                              exclude_label = "wontfix",
                              exclude_issues = NULL,
                              exclude_not_planned = TRUE,
                              alphabetical = FALSE,
                              check_urls = TRUE,
                              quiet = FALSE) {

    if (!quiet) {
        cat (cli::col_cyan (cli::symbol$star), " Extracting code contributors")
        utils::flush.console ()
    }

    ctb_code <- get_gh_code_contributors (
        org,
        repo,
        alphabetical = alphabetical
    )
    ctb_code <- ctb_code [which (!is.na (ctb_code$login)), ]
    ctb_code$type <- "code"
    if (!quiet) {
        message (
            "\r", cli::col_green (cli::symbol$tick),
            " Extracted code contributors   "
        )
    }

    issue_authors <- issue_contributors <- NULL
    if ("issues" %in% type) {
        if (!quiet) {
            cat (
                cli::col_cyan (cli::symbol$star),
                " Extracting github issue contributors"
            )
            utils::flush.console ()
        }
        ctb_issues <- get_gh_issue_people (
            org = org,
            repo = repo,
            exclude_label = exclude_label,
            exclude_issues = exclude_issues,
            exclude_not_planned = exclude_not_planned
        )

        index <- which (!ctb_issues$authors$login %in% ctb_code$logins)
        ctb_issues$authors <- ctb_issues$authors [index, ]

        index <- which (
            !ctb_issues$contributors$login %in%
                c (ctb_code$logins, ctb_issues$authors$login)
        )
        ctb_issues$contributors <- ctb_issues$contributors [index, ]

        add_na_contribs <- function (x, type) {
            x <- cbind (x, NA_integer_) [, c (1, 3, 2)]
            names (x) [2] <- "contributions"
            x$type <- type
            return (x)
        }
        if (nrow (ctb_issues$authors) > 0) {
            issue_authors <- add_na_contribs (
                ctb_issues$authors,
                "issue_authors"
            )
        }
        if ("discussion" %in% type && nrow (ctb_issues$contributors) > 0) {
            issue_contributors <- add_na_contribs (
                ctb_issues$contributors,
                "issue_contributors"
            )
        }

        if (!quiet) {
            message (
                "\r", cli::col_green (cli::symbol$tick),
                " Extracted github issue contributors    "
            )
        }
    }

    ctbs <- rbind (ctb_code, issue_authors, issue_contributors)

    if (check_urls) {
        ctbs <- check_github_urls (ctbs, quiet = quiet)
    }
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
#' @family github
#' @export
get_gh_code_contributors <- function (org, repo, alphabetical = FALSE) {

    tok <- get_gh_token ()
    if (length (tok) == 0) {
        tok <- ""
    }

    # This query can not be done via GraphQL, so have to use v3 REST API
    u <- paste0 (
        "https://api.github.com/repos/",
        org,
        "/",
        repo,
        "/contributors"
    )

    per_page <- 100L
    pagenum <- 1L
    params <- list (per_page = per_page, page = pagenum)

    req <- httr2::request (u)
    if (nchar (tok) > 0L) {
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }
    req <- httr2::req_body_json (req, params)
    req <- httr2::req_method (req, "GET")

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    res <- x
    while (length (x) == per_page) {
        params$page <- params$page + 1L
        req <- httr2::req_body_json (req, params)

        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)

        x <- httr2::resp_body_json (resp, simplifyVector = TRUE)
        res <- c (res, x)
    }

    index <- seq_len (nrow (res))
    if (alphabetical) {
        index <- order (res$login)
    }

    data.frame (
        logins = res$login,
        contributions = res$contributions,
        avatar = res$avatar_url,
        stringsAsFactors = FALSE
    ) [index, ]
}

get_gh_token <- function (token = "") {

    tryCatch (
        gitcreds::gitcreds_get ()$password,
        error = function (e) ""
    )
}

get_git_user <- function () {
    # whoami::whoami ()
    cfg <- tryCatch (
        gert::git_config (),
        error = function (e) NULL
    )
    if (is.null (cfg)) {
        cfg <- tryCatch (
            gert::git_config_global (),
            error = function (e) NULL
        )
    }
    out <- cfg$value [cfg$name == "user.name"]
    if (is.null (out)) {
        out <- ""
    }
    return (out)
}


# ********************  GITHUB ISSUE EXTRACTION ********************


get_issues_qry <- function (org, repo, end_cursor = NULL) {
    after_txt <- ""
    if (!is.null (end_cursor)) {
        after_txt <- paste0 (", after:\"", end_cursor, "\"")
    }

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
                                   labels (first: 100) {
                                       edges {
                                           node {
                                               name
                                           }
                                       }
                                   }
                                   stateReason
                                   url
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
#' issues for nominated repository, excluding issues closed as "not planned"
#'
#' @inheritParams get_contributors
#' @return List of (authors, contributors), each as character vector of github
#' login names.
#'
#' @examples
#' \dontrun{
#' get_gh_issue_people (org = "ropenscilabs", repo = "allcontributors")
#' }
#' @family github
#' @export
get_gh_issue_people <- function (org, repo,
                                 exclude_issues = NULL,
                                 exclude_label = "wontfix",
                                 exclude_not_planned = TRUE) {

    has_next_page <- TRUE
    end_cursor <- NULL
    issue_authors <- issue_numbers <- issue_author_avatar <-
        issue_state_reason <- NULL
    issue_contributors <- issue_contributors_avatar <- issue_labels <- list ()

    while (has_next_page) {

        q <- get_issues_qry (
            org = org,
            repo = repo,
            end_cursor = end_cursor
        )

        check_rate_limit()
        dat <- gh::gh_gql (q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges
        issue_numbers <- c (
            issue_numbers,
            vapply (dat, function (i) i$node$number, integer (1L))
        )
        issue_authors <- c (
            issue_authors,
            vapply (dat, function (i) {
                res <- i$node$author$login
                ifelse (is.null (res), NA_character_, res)
            }, character (1L))
        )
        issue_author_avatar <- c (
            issue_author_avatar,
            vapply (dat, function (i) {
                res <- i$node$author$avatarUrl
                ifelse (is.null (res), NA_character_, res)
            }, character (1L))
        )
        issue_state_reason <- c (
            issue_state_reason,
            vapply (dat, function (i) {
                out <- i$node$stateReason
                ifelse (length (out) == 0, NA_character_, out)
            }, character (1L))
        )

        author <- dat$node$participants$edges

        author_login <- lapply (dat, function (i) {
            edges <- i$node$participants$edges
            unlist (lapply (edges, function (j) j$node$login))
        })
        author_avatar <- lapply (dat, function (i) {
            edges <- i$node$participants$edges
            unlist (lapply (edges, function (j) j$node$avatarUrl))
        })

        issue_contributors <- c (issue_contributors, author_login)
        issue_contributors_avatar <- c (
            issue_contributors_avatar,
            author_avatar
        )

        these_labels <- lapply (dat, function (i) {
            out <- i$node$labels$edges
            ifelse (
                length (out) == 0L,
                "",
                vapply (out, function (j) j$node$name, character (1L))
            )
        })
        issue_labels <- c (issue_labels, these_labels)
    }

    # rm any issues closed as "not planned"
    not_planned <- which (issue_state_reason == "NOT_PLANNED")
    if (exclude_not_planned && length (not_planned) > 0L) {
        issue_numbers <- issue_numbers [-not_planned]
        issue_authors <- issue_authors [-not_planned]
        issue_author_avatar <- issue_author_avatar [-not_planned]
        issue_contributors <- issue_contributors [-not_planned]
        issue_contributors_avatar <- issue_contributors_avatar [-not_planned]
    }

    if (!is.null (exclude_issues)) {

        if (any (!exclude_issues %in% issue_numbers)) {
            stop ("exclude_issues extends beyond range of issues in this repo")
        }

        exclude_issues <- which (issue_numbers %in% exclude_issues)
        issue_authors <- issue_authors [-exclude_issues]
        issue_author_avatar <- issue_author_avatar [-exclude_issues]
        issue_contributors <- issue_contributors [-exclude_issues]
        issue_contributors_avatar <- issue_contributors_avatar [-exclude_issues]
    }

    if (is.null (exclude_label)) {
        exclude_label <- ""
    }
    if (nzchar (exclude_label)) {

        index <- which (vapply (issue_labels, function (i) {
            !any (i %in% exclude_label)
        }, logical (1L)))
        issue_authors <- issue_authors [index]
        issue_author_avatar <- issue_author_avatar [index]
        issue_contributors <- issue_contributors [index]
        issue_contributors_avatar <- issue_contributors_avatar [index]
    }

    index <- which (!duplicated (issue_authors) &
        !is.na (issue_authors))
    issue_authors <- issue_authors [index]
    issue_author_avatar <- issue_author_avatar [index]

    issue_contributors <- unlist (issue_contributors)
    issue_contributors_avatar <- unlist (issue_contributors_avatar)
    index <- which (!(duplicated (issue_contributors) |
        issue_contributors %in% issue_authors))
    issue_contributors <- issue_contributors [index]
    issue_contributors_avatar <- issue_contributors_avatar [index]

    list (
        authors = data.frame (
            logins = issue_authors,
            avatar = issue_author_avatar,
            stringsAsFactors = FALSE
        ),
        contributors = data.frame (
            logins = issue_contributors,
            avatar = issue_contributors_avatar,
            stringsAsFactors = FALSE
        )
    )
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
#' @family github
#' @export
get_gh_issue_titles <- function (org, repo) {

    has_next_page <- TRUE
    end_cursor <- NULL
    issue_title <- issue_number <- NULL
    while (has_next_page) {
        q <- get_issues_qry (
            org = org,
            repo = repo,
            end_cursor = end_cursor
        )
        check_rate_limit()
        dat <- gh::gh_gql (q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        dat <- dat$data$repository$issues$edges

        issue_number <- c (
            issue_number,
            vapply (dat, function (i) i$node$number, integer (1L))
        )
        issue_title <- c (
            issue_title,
            vapply (dat, function (i) i$node$title, character (1L))
        )
    }

    data.frame (
        number = issue_number,
        title = issue_title,
        stringsAsFactors = FALSE
    )
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
#' @family github
#' @export
get_gh_contrib_issue <- function (org, repo) {

    # Note that this is much easier with the REST API than via graphql.

    issues <- get_gh_issue_titles (org, repo)
    issue_num <- issues$number [grep ("all contrib", tolower (issues$title))]
    if (length (issue_num) == 0) {
        return (NULL)
    }

    tok <- get_gh_token ()

    u <- paste0 (
        "https://api.github.com/repos/",
        org,
        "/",
        repo,
        "/issues/",
        issue_num
    )

    req <- httr2::request (u)

    if (nchar (tok) > 0) {
        headers <- list (Authorization = paste0 ("Bearer ", tok))
        req <- httr2::req_headers (req, "Authorization" = headers)
    }
    params <- list (state = "all", per_page = 100, page = 1)
    req <- httr2::req_body_json (req, params)
    req <- httr2::req_method (req, "GET")

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)
    cmts <- x$body

    # That's just the body of the opening comment; the following lines extract
    # all subsequent comments:
    req <- httr2::req_url_path_append (req, "comments")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)
    cmts <- c (cmts, x$body)

    pings <- lapply (cmts, function (i) {
        regmatches (i, regexpr ("@[^[:space:]]+", i))
    })

    unlist (pings)
}

#' Check the GitHub rate limit and warn if exceeded.
#'
#' @noRd
check_rate_limit <- function () {
    gh_state <- gh::gh_rate_limit()
    limit_warn <- 0.1
    warn_txt <- NULL
    if (gh_state$remaining / gh_state$limit < limit_warn) {
        warn_txt <- "You have used > 90% of your GitHub calls..."
    } else if (gh_state$remaining == 0) {
        warn_txt <- "The GitHub rate limit has been reached..."
    }
    if (!is.null (warn_txt)) {
        cli::cli_alert_warning (sprintf(
                "%s It resets in ~%s minutes.",
                warn_txt,
                ceiling(difftime(gh_state$reset, Sys.time(), units = "mins"))))
    }
}
