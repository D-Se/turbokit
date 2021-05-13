# Functions to prepare tasks.

#' Insert library calls if package is not yet loaded
#'
#' @description
#' This function is rarely directly called by the user. More often, it is called
#' through the \code{>} syntax of the \code{construct_complex} function and
#' automatically by the \code{%>>%} superpipe.
#'
#' @return library call in script
#' @export
# # #' @examples
# # #' # when manually inserting library calls
# # #' \donttest{toggle_mode(mode_op = "tidyverse")}
# # #' \donttest{boot()}
# #'
# #' # alternatively, the function can be called by using \code{>} or \code{boot}
# #' # shortcuts in the \code{construct_complex} function.
# #' \donttest{construct_complex("boot")}
boot <- function() {
  up <- getOption("turbokit-up")
  if (!"turbokit" %in% .packages() & getOption("turbokit-boot")) {
    library(turbokit)
    if (getOption("turbokit-verbose")) {
      message("caution: package turbokit attached")
    }
  }
  default_up <- {
    l <- vector(mode = "list", length = 9)
    names(l) <- letters[1:9]
    l
  }
  if (!identical(up, default_up, ignore.environment = TRUE,
                 ignore.bytecode = TRUE, ignore.srcref = TRUE)) {
    packages <- as.character(unlist(up))
    packages <- paste0("library(", gsub(".*_(.+)_.*", "\\1", packages), ")")
    invisible(rstudioapi::insertText(packages))
  } else {
    mode <- mode_toggle()$mode
    switch(mode,
      "tidyverse" = invisible(
        rstudioapi::insertText("library(tidyverse)")
      ),
      "tidymodels" = invisible(
        rstudioapi::insertText("library(tidymodels)")
      ),
      "shiny" = invisible(rstudioapi::insertText("library(shiny)")),
      "dev" = invisible(rstudioapi::insertText("library(devtools)"))
    )
  }
}
