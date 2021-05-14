#' Specify personal preference for shortcut functions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function is is much like the well-known library function. It imports the
#' abbreviations of a package and overwrites the existing mode. See \code{?default()}
#' to undo its effects.
#'
#' @param package name of a package.
#' @param pos position to insert package at in list of options. If \code{NULL},
#' a guess to a position is made.
#' @export
#' @return Change in turbokit options, changing the behavior of \code{construct_complex}
#' @examples
#' # add a package to the search tree of construct_coplex function.
#' read(scales)
#'
#' # note: one exception exists in the naming conventions used
#' read(ggplot)
#'
#' # specify a specific location, to determine a prefix to the abbreviation.
#' read(stringr, 3)
read <- function(package, pos = NULL) {
  package <- deparse(substitute(package))
  l <- vector(mode = "list", length = 9L)
  if (!package %in% turbokit_packages) {
    stop("Package not recognized or not (yet) implemented")
  }
  l <- getOption("turbokit-up")
  if (is.null(pos)) {
    pos <- as.numeric(min(which(unlist(lapply(l, rlang::is_empty)) == T)))
  }
  if (!pos < 10 & pos != 0) {
    stop("Position out of range, accepted positions: 1-9")
  }
  if (!is.numeric(pos)) {
    warning("position is not numeric, converting to numeric")
    pos <- as.numeric(pos)
  }
  ### TODO: shift gears -- when up option is changed, shift default numbers one
  # behind the user defined positions?
  l[[pos]] <- create_expression(package = {{ package }})
  options("turbokit-up" = l)
  if (getOption("turbokit-verbose")) {
    cat(paste0(package, " inserted at position ", pos))
  }
}

# Helper to transform character string into turbokit-recognized expression of package expansion
create_expression <- function(package) {
  package <- rlang::ensym(package)
  term <- rlang::parse_expr(
    paste0(
      "expand_",
      rlang::expr_text(package),
      "_abbreviation(x)"
    )
  )
}

#' Restore default package options used for construct_complex package
#' abbreviations layout.
#'
#' @description
#' This function undos the effects of the \code{read()} function.
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
