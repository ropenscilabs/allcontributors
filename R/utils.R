
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
        if (length (index) > 0)
            ret <- floor (stats::median (nchar (x [index])))
    }
    return (ret)
}
