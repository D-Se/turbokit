# Functions to clean up documents after turbokit usage.

#' Clean the script of turbokit syntax
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Turbokit comes with several features that require special syntax.
#' Remnants of such syntax are not useful to the data science process. The clean
#' function is an exit function that transforms any sentences with tidymath
#' format into comments so that it will not be evaluated when running the script.
#' This is especially helpful for markdown files.
#'
#' Additionally, it detaches the turbokit package from the namespace.
#'
#' @param detach Boolean - detach the turbokit package after cleaning?
#' @export
#' @return Commented turbokit syntax
clean <- function(detach = TRUE) {
  x <- rstudioapi::getActiveDocumentContext()
  tryCatch(expr = {
    rstudioapi::sendToConsole(cleaner(doc = x, detach = detach),
      execute = TRUE, focus = FALSE
    )
  }, error = function(cond) {
    # rstudioapi code param returns error (is not character), but works
    return(NULL)
  })
}

cleaner <- function(doc, detach) {
  clean_ind <- which(stringi::stri_detect(
    str = doc$contents,
    regex = "%>>%"
  ) == T)
  for (i in seq_along(clean_ind)) {
    rstudioapi::modifyRange(
      id = doc$id,
      text = "# ",
      location = rstudioapi::document_position(
        row = clean_ind[i], column = 1
      )
    )
  }
  if (detach & requireNamespace("devtools", quietly = TRUE)) {
    devtools::unload("turbokit")
  }
}
