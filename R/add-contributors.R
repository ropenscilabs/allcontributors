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
#' @param force_update If `TRUE`, update the specified files even if
#' contributions have not changed.
#'
#' @note Opening an issue on github requires the github command-line interface
#' to be locally installed. See \url{https://cli.github.com/}.
#'
#' @return Named list of logical values indicating whether files of given names
#' were updated or not is returned invisibly (that is, only if explicitly
#' assigned to a return value).
#'
#' @examples
#' # The following code extracts the contributors from the git repository
#' # associated with current working directory and writes them to a file.
#' \dontrun{
#' f <- tempfile (fileext = ".Rmd")
#' writeLines ("", f) # blank file in tempdir()
#' add_contributors (repo = ".", files = f)
#' }
#' @export
add_contributors <- function (repo = ".",
                              ncols = 7,
                              files = c ("README.Rmd", "README.md"),
                              type = c ("code", "issues", "discussion"),
                              num_sections = 3,
                              section_names = c ("Code",
                                                 "Issue Authors",
                                                 "Issue Contributors"),
                              format = "grid",
                              alphabetical = FALSE,
                              open_issue = FALSE,
                              force_update = FALSE) {

    if (!git2r::in_repository (repo))
        stop ("The path [", repo, "] does not appear to be a git repository")

    if (identical (section_names,
                   c ("Code", "Issue Authors", "Issue Contributors")) &
        num_sections < 3)
    {
        if (num_sections == 1)
            section_names <- rep ("", 3)
        if (num_sections == 2)
            section_names <- c ("Code", "Issues", "Issues")
    } else if (length (section_names) > num_sections)
        stop ("section_names can not have more entries than num_sections")

    type <- match_type_arg (type)

    format <- match.arg (tolower (format), c ("grid", "list", "text"))

    or <- get_org_repo (repo)

    ctbs <- get_contributors (or$org,
                              or$repo,
                              type = type,
                              alphabetical = alphabetical,
                              quiet = FALSE)

    ctbs$type_name <- section_names [match (ctbs$type,
                                            c ("code",
                                               "issue_authors",
                                               "issue_contributors"))]

    attr (ctbs, "num_sections") <- min (num_sections, length (type),
                                        length (unique (ctbs$type)))

    ctbs <- rename_default_sections (ctbs)

    chk <- add_contribs_to_files (ctbs, or, ncols, format, files,
                                  open_issue, force_update)

    invisible (unlist (chk))
}

match_type_arg <- function (type) {
    if (length (type) > 3)
        stop (paste0 ("There are only three possible types: ",
                      "code, issues, and discussion"))
    c ("code", "issues", "discussion") [seq (length (type))]
}

get_org_repo <- function (repo) {
    remote <- git2r::remote_url (repo)
    remote <- remote [grep ("github", remote)] [1]

    if (length (remote) != 1)
        stop ("Repository must have github remote")

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

add_contribs_to_files <- function (ctbs, orgrepo, ncols, format, files,
                                   open_issue = FALSE,
                                   force_update = FALSE) {

    #files <- file.path (here::here(), files)
    files <- files [which (file.exists (files))]
    files <- files [grep ("\\.md$|\\.Rmd$", files)]

    if (length (files) == 0)
        stop ("None of theose files exist, or are either '.Rmd' or '.md' files")

    current_ctbs <- lapply (files, function (i)
                            get_current_contribs (i, orgrepo))

    chk <- rep (FALSE, length (files))

    for (i in seq_along (files)) {

        new_ctbs <- any (!ctbs$logins %in% current_ctbs [[i]])

        if (force_update | new_ctbs) {

            if (new_ctbs & open_issue) {
                newctbs <- ctbs [which (!ctbs$logins %in% current_ctbs [[i]]), ]
                pinged <- get_gh_contrib_issue (orgrepo$org, orgrepo$repo)
                if (length (pinged) == 0) {
                    open_allcontribs_issue (orgrepo$org, orgrepo$repo, newctbs)
                } else {
                    newctbs <- newctbs [which (!newctbs$logins %in% pinged)]
                    extend_allcontribs_issue (orgrepo$org,
                                              orgrepo$repo,
                                              newctbs)
                }
            }

            chk [i] <- add_contribs_to_one_file (ctbs,
                                                 orgrepo = orgrepo,
                                                 ncols = ncols,
                                                 format = format,
                                                 filename = files [i])
        } else {
            this_file <- utils::tail (strsplit (files [i],
                                                .Platform$file.sep) [[1]], 1)
            message (cli::col_green (cli::symbol$tick),
                     " All current contributors already listed for [",
                     this_file,
                     "]")
        }
    } # end for i over files

    names (chk) <- vapply (files, function (i)
                           utils::tail (strsplit (i, "/") [[1]], 1),
                           character (1), USE.NAMES = FALSE)

    return (chk)
}

add_contribs_to_one_file <- function (ctbs, orgrepo, ncols, format, filename) {

    x <- readLines (filename)

    contribs_sec <- length (x)
    if (!has_contribs_sec (x)) {
        fshort <- utils::tail (strsplit (filename, "/") [[1]], 1)
        message ("File [", fshort, "] has no section titled 'Contributors'; ",
                 "Table will be added to bottom of file.")
    } else {
        contribs_sec <- grep ("^\\#\\#\\sContributors$|^Contributors$", x)
        if (x [contribs_sec + 1] == "^-+$")
            contribs_sec <- contribs_sec + 1
        if (x [contribs_sec + 1] == "")
            contribs_sec <- contribs_sec + 1
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
    if (!has_contribs_sec (x)) {
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
               paste0 ("<!-- ALL-CONTRIBUTORS-LIST:START - ",
                       "Do not remove or modify this section -->"),
               "<!-- prettier-ignore-start -->",
               "<!-- markdownlint-disable -->",
               "",
               paste0 ("All contributions to this project are ",
                       "gratefully acknowledged using the ",
                       "[`allcontributors` package]",
                       "(https://github.com/ropenscilabs/allcontributors)",
                       " following the ",
                       "[all-contributors](https://allcontributors.org) ",
                       "specification. ",
                       "Contributions of any kind are welcome!"))

    num_sections <- attr (ctbs, "num_sections")
    if (num_sections == 1) {
        xmid <- c (xmid, add_one_section (ctbs, orgrepo, ncols,
                                          type = ctbs$type [1], format))
    } else {
        ctbs <- split (ctbs, as.factor (ctbs$type_name))
        for (i in ctbs) {
            type_namei <- tools::toTitleCase (gsub ("\\_", " ",
                                                    i$type_name [1]))

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


add_one_section <- function (ctbs, orgrepo, ncols,
                             type = "code", format = "grid") {

    if (format == "grid") {
        nmax <- ceiling (nrow (ctbs) / ncols)
        index <- rep (1:nmax, each = ncols) [seq (nrow (ctbs))]
        ctbs <- split (ctbs, as.factor (index))
    } else
        ctbs <- list (ctbs)
    x <- ""

    if (format == "grid")
        x <- c (x, "<table>")
    else if (format == "list")
        x <- c (x, "<ol>")

    for (i in ctbs) {
        x <- c (x, "")
        if (format == "grid")
            x <- c (x, "<tr>")

        for (j in seq (nrow (i))) {
            href <- NULL
            u <- paste0 ("<a href=\"https://github.com/",
                         orgrepo$org,
                         "/",
                         orgrepo$repo)
            href <- href_from_type (u, i, j, type)
            x <- c (x, format_contribs (href, i, j, format))
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

href_from_type <- function (u, i, j, type) {

    href <- NULL

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

    return (href)
}

format_contribs <- function (href, i, j, format) {

    ret <- NULL

    if (format == "grid") {
        ret <- c ("<td align=\"center\">",
                  paste0 ("<a href=\"https://github.com/",
                          i$logins [j],
                          "\">"),
                  paste0 ("<img src=\"",
                          i$avatar [j],
                          "\" width=\"100px;\" alt=\"\"/>"),
                  "</a><br>",
                  href,
                  "</td>")
    } else if (format == "list") {
        ret <- paste0 ("<li>", href, "</li>")
    } else {
        ret <- paste0 (href, ifelse (j == nrow (i), "", ", "))
    }

    return (ret)
}

rename_default_sections <- function (ctbs) {
    default_type_names <- c ("Code", "Issue Authors", "Issue Contributors")
    if (all (ctbs$type_name %in% default_type_names)) {
        num_sections <- attr (ctbs, "num_sections")
        if (num_sections == 1)
            ctbs$type_name <- ""
        else if (num_sections == 2)
            ctbs$type_name [ctbs$type_name %in%
                            default_type_names [2:3]] <- "Issues"
    }

    return (ctbs)
}
