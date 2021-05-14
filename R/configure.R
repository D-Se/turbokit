#' Configure turbokit options through menuing
#'
#' @description
#' Interactively toggle between turbokit package options in an R session. When
#' selected, switches the options to \code{TRUE} or \code{FALSE}, depending on
#' the initial setting. Use \emph{Ctrl + mouse} to select multiple options at once.
#' See \code{vignette("turbokit") for explanation of options. }
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
