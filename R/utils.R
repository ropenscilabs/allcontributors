#' Does a file already have a "Contributors" section?
#' @param x A character vector with the contents of a .Rmd/.md file
#' @return Logical variable indicating whether file already has a contributors
#' section.
#' @noRd
has_contribs_sec <- function (x) {

    contribs_sec <- grep ("# Contributors$", x)
    has_contribs_sec <- length (contribs_sec) == 1
    if (!has_contribs_sec) {
        contribs_sec <- grep ("^Contributors$", x)
        title_sec <- grep ("^-+$", x)
        has_contribs_sec <- any ((title_sec - contribs_sec) == 1)
    }

    return (has_contribs_sec)
}

#' Determine whether markdown sections are single-line hash-format, or two-line
#' ["title", "---"]-format.
#' @inheritParams has_contribs_sec
#' @return Integer value of 0 if single-line has format, otherwise median number
#' of symbols used in header delineators
#' @noRd
section_format <- function (x) {
    index <- grep ("^#", x)
    uses_hash <- length (index) > 1 & any (x [index + 1] == "")
    ret <- 0
    if (!uses_hash) {
        index <- grep ("^-+$", x)
        if (length (index) > 0) {
            ret <- floor (stats::median (nchar (x [index])))
        }
    }
    return (ret)
}

#' A `gert` version of `git2r::in_repository()`.
#'
#' See https://github.com/ropensci/allcontributors/issues/27
#' @noRd
in_git_repository <- function (path = ".") {

    res <- tryCatch (
        gert::git_info (),
        error = function (e) NULL
    )

    return (!is.null (res))
}

# from usethis
# https://github.com/r-lib/usethis/blob/26a843c148ded63300f3d17f6fba899611640aac/R/utils-github.R#L38
parse_github_remotes <- function (x) {
    # https://github.com/r-lib/usethis
    #       --> https, github.com,      rlib, usethis
    # https://github.com/r-lib/usethis.git
    #       --> https, github.com,      rlib, usethis
    # https://github.com/r-lib/usethis#readme
    #       --> https, github.com,      rlib, usethis
    # https://github.com/r-lib/usethis/issues/1169
    #       --> https, github.com,      rlib, usethis
    # https://github.acme.com/r-lib/devtools.git
    #       --> https, github.acme.com, rlib, usethis
    # git@github.com:r-lib/usethis.git
    #       --> ssh,   github.com,      rlib, usethis
    # ssh://git@github.com/rstudio/packrat.git
    #       --> ssh,   github.com,      rlib, usethis
    dat <- re_match (x, github_remote_regex)

    dat$protocol <- sub ("://$", "", dat$protocol)
    dat$user <- sub ("@$", "", dat$user)
    dat$repo_name <- sub ("[.]git$", "", dat$repo_name)
    dat$url <- dat$.text

    # as.character() necessary for edge case of length-0 input
    dat$protocol <-
        as.character (ifelse (dat$protocol == "https", "https", "ssh"))
    dat$name <- if (!is.null (names (x))) {
        names (x)
    } else {
        rep_len (NA_character_, length.out = nrow (dat))
    }

    dat [c ("name", "url", "host", "repo_owner", "repo_name", "protocol")]
}

# https://github.com/r-lib/usethis/blob/26a843c148ded63300f3d17f6fba899611640aac/R/utils-rematch2.R#L5
re_match <- function (text, pattern, perl = TRUE, ...) {

    stopifnot (is.character (pattern), length (pattern) == 1, !is.na (pattern))
    text <- as.character (text)

    match <- regexpr (pattern, text, perl = perl, ...)

    start <- as.vector (match)
    length <- attr (match, "match.length")
    end <- start + length - 1L

    matchstr <- substring (text, start, end)
    matchstr [start == -1] <- NA_character_

    res <- data.frame (
        stringsAsFactors = FALSE,
        .text = text,
        .match = matchstr
    )

    if (!is.null (attr (match, "capture.start"))) {

        gstart <- attr (match, "capture.start")
        glength <- attr (match, "capture.length")
        gend <- gstart + glength - 1L

        groupstr <- substring (text, gstart, gend)
        groupstr [gstart == -1] <- NA_character_
        dim (groupstr) <- dim (gstart)

        res <- cbind (groupstr, res, stringsAsFactors = FALSE)
    }

    names (res) <- c (attr (match, "capture.names"), ".text", ".match")
    # class(res) <- c("tbl_df", "tbl", class(res))
    res
}

# https://github.com/r-lib/usethis/blob/26a843c148ded63300f3d17f6fba899611640aac/R/utils-github.R#L25
# named vector or list of GitHub URLs --> data frame of URL parts
# more general than the name suggests
# definitely designed for GitHub URLs but not overtly GitHub-specific
# https://stackoverflow.com/questions/2514859/regular-expression-for-git-repository
# https://git-scm.com/docs/git-clone#_git_urls
# https://stackoverflow.com/questions/27745/getting-parts-of-a-url-regex
github_remote_regex <- paste0 (
    "^",
    "(?<protocol>\\w+://)?",
    "(?<user>.+@)?",
    "(?<host>[^/:]+)",
    "[/:]",
    "(?<repo_owner>[^/]+)",
    "/",
    "(?<repo_name>[^/#]+)",
    "(?<fragment>.*)",
    "$"
)
