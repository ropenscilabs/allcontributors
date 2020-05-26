#' add_contributors
#'
#' Add contributors to README.Rmd
#'
#' @param ncols Number of columns for contributors in 'README'
#' @param alphabetical If `TRUE`, order contributors alphabetically, otherwise
#' order by decreasing numbers of contributions.
#' @return Nothing.
#' @export
add_contributors <- function (ncols = 7, alphabetical = FALSE) {
    if (!git2r::in_repository ())
        stop ("This does not appear to be a git repository")

    remote <- git2r::remote_url ()
    remote <- remote [grep ("github", remote)] [1]

    if (length (remote) != 1)
        stop ("Repository must have github remote")

    or <- get_org_repo (remote)
    x <- get_contributors (or$org, or$repo, alphabetical = alphabetical)

    x <- x [which (!is.na (x$login)), ]
    contribs_to_readme (x, orgrepo = or, ncols = ncols, rmd = TRUE)
    contribs_to_readme (x, orgrepo = or, ncols = ncols, rmd = FALSE)
}

get_org_repo <- function (remote) {
    org <- utils::tail (strsplit (remote, "/") [[1]], 2) [1]
    repo <- utils::tail (strsplit (remote, "/") [[1]], 1) [1]
    list (org = org,
          repo = repo)
}

contribs_to_readme <- function (dat, orgrepo, ncols, rmd = TRUE) {
    if (rmd)
        f0 <- "README.Rmd"
    else
        f0 <- "README.md"

    f <- file.path (here::here (), f0)
    if (!file.exists (f))
        return (NULL)
        #stop ("Unable to find ", f0)

    x <- readLines (f)

    contribs_sec <- grep ("# Contributors$", x)
    if (length (contribs_sec) != 1)
        stop ("README must have a section titled 'Contributors'")

    contribs_start <- grep ("<!-- ALL-CONTRIBUTORS-LIST:START", x)
    contribs_end <- grep ("<!-- ALL-CONTRIBUTORS-LIST:END", x)
    if (length (contribs_start) == 1 & length (contribs_end) == 1) {
        xtop <- x [1:(contribs_start - 1)]
        xbottom <- x [(contribs_end + 1):length (x)]
        
    } else {
        xtop <- x [1:contribs_sec]
        xbottom <- x [(contribs_sec + 1):length (x)]
    }

    xmid <- c ("<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->",
               "<!-- prettier-ignore-start -->",
               "<!-- markdownlint-disable -->",
               "",
               paste0 ("This project follows the ",
                       "[all-contributors](https://allcontributors.org) specification. ",
                       "Contributions of any kind are welcome!"),
               "",
               "<table>")

    nmax <- ceiling (nrow (dat) / ncols)
    index <- rep (1:nmax, each = ncols) [seq (nrow (dat))]
    dat <- split (dat, as.factor (index))
    for (i in dat) {
        xmid <- c (xmid,
                   "",
                   "<tr>")

        for (j in seq (nrow (i))) {
            xmid <- c (xmid,
                       "<td align=\"center\">",
                       paste0 ("<a href=\"https://github.com/", i$login [j], "\">"),
                       paste0 ("<img src=\"", i$avatars [j], "\" width=\"100px;\" alt=\"\"/>"),
                       "</a><br>",
                       paste0 ("<a href=\"https://github.com/",
                               orgrepo$org,
                               "/",
                               orgrepo$repo,
                               "/commits?author=",
                               i$login [j],
                               "\">",
                               i$login [j],
                               "</a>"),
                       "</td>")
        }

        xmid <- c (xmid,
                   "</tr>",
                   "")

    }


    xmid <- c (xmid,
               "</table>",
               "",
               "<!-- markdownlint-enable -->",
               "<!-- prettier-ignore-end -->",
               "<!-- ALL-CONTRIBUTORS-LIST:END -->")

    txt <- c (xtop, xmid, xbottom)
    con <- file (f, "w")
    writeLines (txt, con = con)
    close (con)

    message ("contributors written to [", f, "]")
}

