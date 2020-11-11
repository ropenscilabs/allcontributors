#' add_contributors
#'
#' Add contributors to README.Rmd
#'
#' @param repo Location of repository for which contributions are to be
#' extracted. This must be a git project with a github remote.
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
#' @param section_names Names of the sections to appear on the nominated
#' `files`.
#' @param alphabetical If `TRUE`, order contributors alphabetically, otherwise
#' order by decreasing numbers of contributions.
#' @param format One of ("grid", "list", "text") to control display of
#' contributors as
#' \itemize{
#' \item{1} "grid" for a rectangular grid, with each contributor represented by
#' their github avatar, with avatar linked to contributor profile and name
#' linked to repository contributions.
#' \item{2} "list" for a more condensed list with github user names only
#' and no avatars, one contributor per line linked to issue contributions.
#' \item{3} "text" for a single line of text containing comma-separated github
#' user names linked to issue contributions.
#' }
#' @param open_issue If `TRUE`, open or edit an issue on github in order to
#' notify all contributors that they've been added to your `README` (see Note).
#'
#' @note Opening an issue on github requires the github command-line interface
#' to be locally installed. See \url{https://cli.github.com/}.
#'
#' @return Named list of logical values indicating whether files of given names
#' were updated or not is returned invisibly (that is, only if explicitly
#' assigned to a return value).
#' @export
add_contributors <- function (repo = ".",
                              ncols = 7,
                              files = c ("README.Rmd", "README.md"),
                              type = c ("code", "issues", "discussion"),
                              num_sections = 3,
                              section_names = c ("Code", "Issue Authors", "Issue Contributors"),
                              format = "grid",
                              alphabetical = FALSE,
                              open_issue = FALSE) {

    if (!git2r::in_repository (repo))
        stop ("The path [", repo, "] does not appear to be a git repository")

    type <- match_type_arg (type)

    format <- match.arg (tolower (format), c ("grid", "list", "text"))

    remote <- git2r::remote_url (repo)
    remote <- remote [grep ("github", remote)] [1]

    if (length (remote) != 1)
        stop ("Repository must have github remote")

    or <- get_org_repo (remote)
    message (cli::col_cyan (cli::symbol$star), 
             " Extracting code contributors", appendLF = FALSE)
    ctb_code <- get_contributors (or$org,
                                  or$repo,
                                  alphabetical = alphabetical)
    message ("\r", cli::col_green (cli::symbol$tick), " Extracted code contributors")

    ctb_code <- ctb_code [which (!is.na (ctb_code$login)), ]
    ctb_code$type <- "code"

    issue_authors <- issue_contributors <- NULL
    if ("issues" %in% type) {
        message (cli::col_cyan (cli::symbol$star), 
                 " Extracting github issue contributors", appendLF = FALSE)
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
        message ("\r", cli::col_green (cli::symbol$tick),
                 " Extracted github issue contributors")
    }

    ctbs <- rbind (ctb_code, issue_authors, issue_contributors)

    attr (ctbs, "num_sections") <- min (num_sections, length (type),
                                        length (unique (ctbs$type)))

    ctbs$type_name <- section_names [match (ctbs$type,
                                            c ("code",
                                               "issue_authors",
                                               "issue_contributors"))]

    files <- file.path (here::here(), files)
    files <- files [which (file.exists (files))]
    files <- files [grep ("\\.md$|\\.Rmd$", files)]

    current_ctbs <- lapply (files, function (i) get_current_contribs (i, or))

    chk <- rep (FALSE, length (files))

    for (i in seq (files)) {

        if (any (!ctbs$logins %in% current_ctbs [[i]])) {

            if (open_issue) {
                newctbs <- ctbs [which (!ctbs$logins %in% current_ctbs [[i]]), ]
                pinged <- get_gh_contrib_issue (or$org, or$repo)
                if (length (pinged) == 0) {
                    open_allcontribs_issue (or$org, or$repo, newctbs)
                } else {
                    newctbs <- newctbs [which (!newctbs$logins %in% pinged)]
                    extend_allcontribs_issue (or$org, or$repo, newctbs)
                }
            }

            # code contributions to files:
            chk [i] <- add_contribs_to_file (ctbs,
                                             orgrepo = or,
                                             ncols = ncols,
                                             format = format,
                                             filename = files [i])
        } else {
            this_file <- utils::tail (strsplit (files [i], .Platform$file.sep) [[1]], 1)
            message (cli::col_green (cli::symbol$tick),
                     " All current contributors already listed for [",
                     this_file,
                     "]")
        }
    } # end for i over files

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

# strip current list of contributors from filename
get_current_contribs <- function (filename, orgrepo) {
    x <- readLines (filename)

    ghurl <- paste0 ("https://github.com/",
                     orgrepo$org,
                     "/",
                     orgrepo$repo,
                     "/")
    # terminal slash important, because text generally also has link to repo
    # without slash

    res <- NULL # default if no contributors present

    if (has_contribs_sec (x)) {

        contribs_start <- grep ("<!-- ALL-CONTRIBUTORS-LIST:START", x)
        contribs_end <- grep ("<!-- ALL-CONTRIBUTORS-LIST:END", x)
        if (length (contribs_start) == 1 & length (contribs_end) == 1) {
            res <- x [(contribs_start + 1):(contribs_end - 1)]
            res <- res [grep (ghurl, res)]
            res <- vapply (strsplit (res, "\">"), function (i)
                           strsplit (i [2], "</a>") [[1]] [1], character (1))
        }
    }

    return (res)
}

add_contribs_to_file <- function (dat, orgrepo, ncols, format, filename) {
    x <- readLines (filename)

    if (!has_contribs_sec (x)) {
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
    if (!has_contribs_sec (x))
    {
        sec_fmt <- section_format (x)
        if (sec_fmt == 0)
            xmid <- "## Contributors"
        else
            xmid <- c ("Contributors",
                       paste0 (rep ("-", sec_fmt), collapse = ""))
        xmid <- c ("", xmid, "")
    }

    xmid <- c (xmid,
               "",
               "<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->",
               "<!-- prettier-ignore-start -->",
               "<!-- markdownlint-disable -->",
               "",
               paste0 ("This project uses the ",
                       "[`allcontributors` package](https://github.com/ropenscilabs/allcontributors)",
                       " following the ",
                       "[all-contributors](https://allcontributors.org) specification. ",
                       "Contributions of any kind are welcome!"))

    num_sections <- attr (dat, "num_sections")
    if (num_sections == 1) {
        xmid <- c (xmid, add_one_section (dat, orgrepo, ncols,
                                          type = dat$type [1], format))
    } else {
        if (num_sections < 3) {
            # types are always sorted (code, issue authors, issue contributors)
            type_names <- unique (dat$type_name)
            dat$type_name [dat$type_name == type_names [3]] <- type_names [2]
        }

        dat <- split (dat, as.factor (dat$type))
        for (i in dat) {
            type_namei <- tools::toTitleCase (gsub ("\\_", " ", i$type_name [1]))

            xmid <- c (xmid, "", paste0 ("### ", type_namei))

            xmid <- c (xmid, add_one_section (i, orgrepo, ncols,
                                              i$type [1],
                                              format))
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


add_one_section <- function (dat, orgrepo, ncols,
                             type = "code", format = "grid") {

    if (format == "grid") {
        nmax <- ceiling (nrow (dat) / ncols)
        index <- rep (1:nmax, each = ncols) [seq (nrow (dat))]
        dat <- split (dat, as.factor (index))
    } else
        dat <- list (dat)
    x <- ""

    if (format == "grid")
        x <- c (x, "<table>")
    else if (format == "list")
        x <- c (x, "<ol>")

    for (i in dat) {
        x <- c (x, "")
        if (format == "grid")
            x <- c (x, "<tr>")

        for (j in seq (nrow (i))) {
            href <- NULL
            u <- paste0 ("<a href=\"https://github.com/",
                         orgrepo$org,
                         "/",
                         orgrepo$repo)
            if (type == "code") {
                href <- paste0 (u, 
                                "/commits?author=",
                                i$logins [j],
                                "\">",
                                i$logins [j],
                                "</a>")
            } else if (type == "issue_authors") {
                href <- paste0 (u,
                                "/issues?q=is%3Aissue+author%3A",
                                i$logins [j],
                                "\">",
                                i$logins [j],
                                "</a>")
            } else if (type == "issue_contributors") {
                href <- paste0 (u,
                                "/issues?q=is%3Aissue+commenter%3A",
                                i$logins [j],
                                "\">",
                                i$logins [j],
                                "</a>")
            }
            if (format == "grid") {
                x <- c (x,
                        "<td align=\"center\">",
                        paste0 ("<a href=\"https://github.com/", i$logins [j], "\">"),
                        paste0 ("<img src=\"", i$avatar [j], "\" width=\"100px;\" alt=\"\"/>"),
                        "</a><br>",
                        href,
                        "</td>")
            } else if (format == "list") {
                x <- c (x, paste0 ("<li>", href, "</li>"))
            } else {
                x <- c (x, paste0 (href, ifelse (j == nrow (i), "", ", ")))
            }
        }

        if (format == "grid")
            x <- c (x, "</tr>")
        x <- c (x, "")

    }

    if (format == "grid")
        x <- c (x, "</table>")
    else if (format == "list")
        x <- c (x, "</ol>")

    x <- c (x, "")

    return (x)
}
