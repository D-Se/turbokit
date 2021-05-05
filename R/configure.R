#' Restore default package options used for construct_complex package
#' abbreviations layout.
#'
#' @description
#' This function undos the action of the read function.
#'
#' @export
#' @return library call in script
#' @examples
#' # resetting the default
#' default()
default <- function() {
    ### TODO:withr here?
    options("turbokit-up" = {
        l <- vector(mode = "list", length = 9)
        names(l) <- letters[1:9]
        l
    })
}
