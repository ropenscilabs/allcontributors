
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
