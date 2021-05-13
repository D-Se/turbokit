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


#' Configure turbokit options through menuing
#'
#' @description
#' Interactively change package options in an R session. Use \emph{Ctrl + mouse}
#' to select multiple options.
#'
#' @export
#' @return library call in script
#' @examples
#' # resetting the default
#' configure_turbokit()
configure_turbokit <- function(){
  opts <- svDialogs::dlg_list(
    choices = c("smartpipe", "autoinsert", "verbose", "boot"),
    multiple = TRUE, preselect = NULL)
  if ("boot" %in% opts$res) {
    current <- getOption("turbokit-boot")
    ifelse(current,
           options("turbokit-boot" = FALSE),
           options("turbokit-boot" = TRUE))
  }
  if ("smartpipe" %in% opts$res) {
    current <- getOption("turbokit-smartpipe")
    ifelse(current,
           options("turbokit-smartpipe" = FALSE),
           options("turbokit-smartpipe" = TRUE))
  }
  if ("autoinsert" %in% opts$res) {
    current <- getOption("turbokit-autoinsert")
    ifelse(current,
           options("turbokit-autoinsert" = FALSE),
           options("turbokit-autoinsert" = TRUE))
  }
  if ("verbose" %in% opts$res) {
    current <- getOption("turbokit-verbose")
    ifelse(current,
           options("turbokit-verbose" = FALSE),
           options("turbokit-verbose" = TRUE))
  }
}



