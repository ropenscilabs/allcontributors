#' add_contributors
#'
#' Add contributors to README.Rmd
#'
#' @param ncols Number of columns for contributors in 'README'
#' @param files Names of files in which to add contributors
#' @param type Type of contributions to include: 'code' for direct code
#' contributions (including documentation), 'issues' to recognise contributors
#' who open issues, and 'discussion' for contributing to discussions within
#' issues. Discussion contributions are only from individuals not present in
#' either 'issues' or 'code'; and 'issues' contributions are only from
#' individuals not present in 'code'.
#' @param num_sections Number of sections in which to divide contributors:
#' \itemize{
#' \item{1} All contributions within single section regardless of `type`
#' \item{2} Contributions divided between a single section for `code` and a
#' second section for all other issue-related contributions.
#' \item{3} Contributions divided into single sections for each of the three
#' `type` arguments.
#' }
#' @param alphabetical If `TRUE`, order contributors alphabetically, otherwise
#' order by decreasing numbers of contributions.
#' @return Named list of logical values indicating whether files of given names
#' were updated or not is returned invisibly (that is, only if explicitly
#' assigned to a return value).
#' @export
add_contributors <- function (ncols = 7,
                              files = c ("README.Rmd", "README.md"),
                              type = c ("code", "issues", "discussion"),
                              num_sections = 3,
                              alphabetical = FALSE) {
    if (!git2r::in_repository ())
        stop ("This does not appear to be a git repository")

    type <- match_type_arg (type)

    remote <- git2r::remote_url ()
    remote <- remote [grep ("github", remote)] [1]

    if (length (remote) != 1)
        stop ("Repository must have github remote")

    or <- get_org_repo (remote)
    ctb_code <- get_contributors (or$org,
                                  or$repo,
                                  alphabetical = alphabetical)

    ctb_code <- ctb_code [which (!is.na (ctb_code$login)), ]
    ctb_code$type <- "code"

    issue_authors <- issue_contributors <- NULL
    if ("issues" %in% type) {
        ctb_issues <- get_gh_issue_people (org = or$org, repo = or$repo)

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
            issue_authors <- add_na_contribs (ctb_issues$authors, "issue_authors")
        if ("discussion" %in% type & nrow (ctb_issues$contributors) > 0)
            issue_contributors <- add_na_contribs (ctb_issues$contributors,
                                                   "issue_contributors")
    }

    ctbs <- rbind (ctb_code, issue_authors, issue_contributors)

    attr (ctbs, "num_sections") <- min (num_sections, length (type),
                                        length (unique (ctbs$type)))

    files <- file.path (here::here(), files)
    files <- files [which (file.exists (files))]

    # code contributions to files:
    chk <- lapply (files, function (i)
            contribs_to_readme (ctbs,
                                orgrepo = or,
                                ncols = ncols,
                                filename = i))

    names (chk) <- vapply (files, function (i)
                           utils::tail (strsplit (i, "/") [[1]], 1),
                           character (1), USE.NAMES = FALSE)

    invisible (unlist (chk))
}

match_type_arg <- function (type) {
    if (length (type) > 3)
        stop ("There are only three possible types: code, issues, and discussion")
    c ("code", "issues", "discussion") [seq (length (type))]
}

get_org_repo <- function (remote) {
    org <- utils::tail (strsplit (remote, "/") [[1]], 2) [1]
    repo <- utils::tail (strsplit (remote, "/") [[1]], 1) [1]
    list (org = org,
          repo = repo)
}

contribs_to_readme <- function (dat, orgrepo, ncols, filename) {
    x <- readLines (filename)

    contribs_sec <- grep ("# Contributors$", x)
    has_contribs_sec <- length (contribs_sec) == 1
    if (!has_contribs_sec) {
        fshort <- utils::tail (strsplit (filename, "/") [[1]], 1)
        message ("File [", fshort, "] has no section titled 'Contributors'; ",
                 "Table will be added to bottom of file.")
        contribs_sec <- length (x)
    }

    contribs_start <- grep ("<!-- ALL-CONTRIBUTORS-LIST:START", x)
    contribs_end <- grep ("<!-- ALL-CONTRIBUTORS-LIST:END", x)
    if (length (contribs_start) == 1 & length (contribs_end) == 1) {
        xtop <- x [1:(contribs_start - 1)]
        xbottom <- NULL
        if (contribs_end < length (x))
            xbottom <- x [(contribs_end + 1):length (x)]

    } else {
        xtop <- x [1:contribs_sec]
        xbottom <- NULL
        if (contribs_sec < length (x))
            xbottom <- x [(contribs_sec + 1):length (x)]
    }

    xmid <- NULL
    if (!has_contribs_sec)
        xmid <- c ("", "## Contributors", "")

    xmid <- c (xmid,
               "",
               "<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->",
               "<!-- prettier-ignore-start -->",
               "<!-- markdownlint-disable -->",
               "",
               paste0 ("This project uses the ",
                       "[`allcontributor` package](https://github.com/mpadge/allcontributor)",
                       " following the ",
                       "[all-contributors](https://allcontributors.org) specification. ",
                       "Contributions of any kind are welcome!"))

    num_sections <- attr (dat, "num_sections")
    if (num_sections == 1) {
        xmid <- c (xmid, add_one_section (dat, orgrepo, ncols))
    } else {
        if (num_sections < 3)
            dat$type [dat$type == "issue_contributors"] <- "issue_authors"

        dat <- split (dat, as.factor (dat$type))
        for (i in dat) {
            typei <- tools::toTitleCase (gsub ("\\_", " ", i$type [1]))
            xmid <- c (xmid,
                       "",
                       paste0 ("## ", typei))
            xmid <- c (xmid, add_one_section (i, orgrepo, ncols))
        }
    }
    
    xmid <- c (xmid, "<!-- markdownlint-enable -->",
               "<!-- prettier-ignore-end -->",
               "<!-- ALL-CONTRIBUTORS-LIST:END -->",
               "")

    txt <- c (xtop, xmid, xbottom)

    newlines <- txt [which (!txt %in% x)]
    changed <- any (nchar (newlines) > 0)

    if (changed) {
        con <- file (filename, "w")
        writeLines (txt, con = con)
        close (con)

        message ("contributors written to [", filename, "]")
    }

    return (changed)
}


add_one_section <- function (dat, orgrepo, ncols) {
    nmax <- ceiling (nrow (dat) / ncols)
    index <- rep (1:nmax, each = ncols) [seq (nrow (dat))]
    dat <- split (dat, as.factor (index))
    x <- c ("", "<table>")
    for (i in dat) {
        x <- c (x,
                "",
                "<tr>")

        for (j in seq (nrow (i))) {
            x <- c (x,
                    "<td align=\"center\">",
                    paste0 ("<a href=\"https://github.com/", i$logins [j], "\">"),
                    paste0 ("<img src=\"", i$avatar [j], "\" width=\"100px;\" alt=\"\"/>"),
                    "</a><br>",
                    paste0 ("<a href=\"https://github.com/",
                            orgrepo$org,
                            "/",
                            orgrepo$repo,
                            "/commits?author=",
                            i$logins [j],
                            "\">",
                            i$logins [j],
                            "</a>"),
                    "</td>")
        }

        x <- c (x,
                "</tr>",
                "")

    }
    x <- c (x, "</table>", "")

    return (x)
}
