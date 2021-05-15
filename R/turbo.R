### TODO: parameter expand functions

#' Wrapper constructor function to insert tidyverse functions at the cursor position
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{turbo()} is a wrapper for \code{construct_*} functions. It
#' allows for rapid insertion of many popular functions living in
#' different packages in a shorthand style, reducing time spent inputting and
#' formatting code.
#'
#' It has powerful side effect, which is its formula-like \emph{pipemath}
#' translation mechanism,with which entire pipelines are constructed and converted
#' to \emph{dynamic snippets}.
#' See \code{vignette("construct")} for a list of combination functions.
#' See \code{\link{superpipe}} for guide on constructing dynamic tidyverse snippets.
#'
#'  \strong{Abbreviations}:
#'
#'  A leading digit indicates which package's function to insert. A digit is followed
#'  by a \emph{non-formatted} series of first letters of snake_case segments,
#'  in order of appearance.
#'
#'  \strong{Default values}:
#'
#'  Given default package options see \code{vignette("set_x_default")},
#'  the leading digits refer to the following packages:
#'
#' \tabular{lllll}{
#'  1\tab - searches for \strong{ggplot} functions\cr
#'  2\tab - searches for \strong{stringr} functions\cr
#'  3\tab - searches for \strong{forcats} functions\cr
#'  4\tab - searches for \strong{clock} functions\cr
#'  5\tab - searches for \strong{readr} functions
#' }
#'
#' @section Construct family:
#' The construct_* functions are intended to be called via
#' a user-defined shortcut. There are three reasons for this:
#'
#' 1. It allows for quick chaining of inputs
#' 2. Bypassing a library call at every session
#' 3. Bypass the need to put quotes around the call
#'
#' In a specific use case, the \code{construct_complex()} function is
#' synergistic with the superpipe operator
#'
#' @param input optional character string to convert to script text at cursor location.
#' @return A formatted function inserted at the cursor location
#' @export
turbo <- function(input = NULL) {
  mode <- mode_toggle()$mode
  up <- getOption("turbokit-up")
  default_up <- {
    l <- vector(mode = "list", length = 9)
    names(l) <- letters[1:9]
    l
  }
  if (!identical(up, default_up, ignore.environment = TRUE,
                 ignore.bytecode = TRUE, ignore.srcref = TRUE)) {
    abb <- get_complex_userinput()
    if (abb %in% c("boot", "clean", "read", "default")) {
      return(turbokit_default(abb))
    } else {
      expression <- expand_user_pref(abb)
      rstudioapi::insertText(paste0(expression, "()"))
      reposition(1)
    }
    return(NULL)
  }
  abb <- character(length = 1)
  if (is.null(input)) {
    abb <- get_complex_userinput()
    if (abb == "error" | abb == "warning") {
      return(NULL) # avoid stop() nasty addin error pop-up box
    }
  } else {
    if (!is.character(input)) {
      message("coercing input to character")
      input <- as.character(input)
    }
    abb <- input
  }
  if (abb %in% c("boot", "clean", "read", "default")) {
    return(turbokit_default(abb))
  }
  # translate pipemath
  if (grepl(x = abb, pattern = "^[~]{1}", perl = TRUE)) {
    return(construct_chain(abb))
  } else if (grepl(x = abb, pattern = "^[0]{1}", perl = TRUE)) {
    abb <- sub(x = abb, pattern = ".", replacement = "", perl = TRUE)
    return(expand_user_defined(abb))
  } else {
    if (mode == "tidyverse") {
      if (grepl(x = abb, pattern = "^[[:alpha:]]", perl = TRUE)) {
        expression <- expand_tidyverse_default(abb)
      } else {
        expression <- expand_tidyverse(abb)
      }
    } else if (mode == "tidymodels") {
      expression <- expand_tidymodels(abb)
    } else if (mode == "shiny") {
      if (grepl(x = abb, pattern = "^[[:alpha:]]", perl = TRUE)) {
        abb <- unlist(strsplit(x = abb, split = "", fixed = TRUE))
        expression <- expand_shiny_default(abb)
      } else {
        expression <- expand_shiny(abb)
      }
    } else if (mode == "visualisation") {
      if (grepl(x = abb, pattern = "^[[:alpha:]]", perl = TRUE)) {
        expression <- expand_visualisation_default(abb)
      } else {
        expression <- expand_visualisation(abb)
      }
    } else if (mode == "dev") {
      if (grepl(x = abb, pattern = "^[[:alpha:]]", perl = TRUE)) {
        expression <- expand_dev_default(abb)
      } else {
        expression <- expand_dev(abb)
      }
    }
  }
  if (grepl("NA", expression, perl = T)) {
    message("Unanticipated verb - if issue persist, please file a bug report.")
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

# Transformation step of dialog box input to be construct_* function-friendly
construct_chain <- function(input) {
  mode <- mode_toggle()$mode
  chain <- as.list(strsplit(x = input, "(?<=[>])", perl = TRUE)[[1]])
  chain <- lapply(chain, function(x) {
    unlist(strsplit(x = x, "(?=[!,>*~])", perl = TRUE))
  })
  if (chain[[1]][2] == "!") {
    chain <- unlist(chain)
    n <- sum(stringi::stri_count(str = chain, regex = "!"))
    return(rep("read", n))
  }
  chain <- lapply(chain, function(x) {
    unlist(
      lapply(x, FUN = function(y) {
        if (grepl(x = y, pattern = "^[[:digit:]]", perl = TRUE)) {
          if (mode == "tidyverse") {
            expression <- expand_tidyverse(y)
          } else if (mode == "tidymodels") {
            expression <- expand_tidymodels(y)
          } else if (mode == "shiny") {
            expression <- expand_shiny(y)
          }
          paste0(expression, "()")
        } else if (nchar(y) > 1) {
          expression <- switch(y,
            "sw" = "starts_with",
            "ew" = "ends_with",
            "rj" = "right_join",
            "lj" = "left_join",
            "cw" = "case_when",
            NA
          )
          paste0(expression, "()")
        } else {
          expression <- switch(y,
            ">" = y,
            "*" = y,
            "~" = y,
            "m" = "mutate",
            "f" = "filter",
            "s" = "select",
            "p" = "ggplot",
            "z" = "summarise",
            "a" = "across",
            "e" = "everything",
            "g" = "group_by",
            "," = y,
            NA
          )
          if (startsWith(expression, ",") | startsWith(expression, ">") |
            startsWith(expression, "*") | startsWith(expression, "~")) {
            y
          } else {
            paste0(expression, "()")
          }
        }
      })
    )
  })
  if (mode == "tidyverse") {
    chain <- append(chain, values = list(c("mydata", ">")), after = 0)
    chain[[2]] <- chain[[2]][-1]
  } else {
    chain[[1]] <- chain[[1]][-1]
  }
  chain <- lapply(chain, function(x) {
    if (any(grepl("[*]", x = x, perl = TRUE))) {
      x <- transform_complex_interaction(x)
    }
    sub(x = x, pattern = ">", replacement = "%>%", fixed = TRUE)
  })
  chain <- lapply(
    lapply(chain[!!lengths(chain)], toString),
    function(x) {
      sub(
        x = x, pattern = ",",
        replacement = "",
        fixed = TRUE
      )
    }
  )
  chain <- lapply(chain, function(x) {
    if (any(grepl(
      x = x,
      pattern = "ggplot|geom|facet|position|scale|coord|element|theme",
      perl = TRUE
    ))) {
      x <- sub(
        x = x, pattern = "%>%",
        replacement = "+",
        fixed = TRUE
      )
      x <- sub(
        x = x, pattern = "ggplot\\(\\)",
        replacement = "ggplot(aes())",
        perl = TRUE
      )
    } else {
      x
    }
  })
  # remove any operator left at the end
  chain[[length(chain)]] <- sub(chain[[length(chain)]],
    pattern = "\\+|\\%>\\%",
    replacement = "",
    perl = TRUE
  )
  styler::style_text(sapply(chain[!!lengths(chain)], toString))
}

expand_tidyverse <- function(x) {
  switch(substr(x, start = 1, stop = 1),
    "1" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggplot_abbreviation(x)
    },
    "2" = {
      x <- transform_complex_input(x)
      if (check_plot_context()) invisible(toggle_pipe())
      expand_stringr_abbreviation(x)
    },
    "3" = {
      x <- transform_complex_input(x)
      if (check_plot_context()) invisible(toggle_pipe())
      expand_forcats_abbreviation(x)
    },
    "4" = {
      x <- transform_complex_input(x)
      if (check_plot_context()) invisible(toggle_pipe())
      expand_clock_abbreviation(x)
    },
    "5" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_readr_abbreviation(x)
    },
    NA
  )
}

expand_tidymodels <- function(x) {
  switch(substr(x, start = 1, stop = 1),
    "1" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_recipes_abbreviation(x)
    },
    "2" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_parsnip_abbreviation(x)
    },
    "3" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_tune_abbreviation(x)
    },
    "4" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_dials_abbreviation(x)
    },
    "5" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_yardstick_abbreviation(x)
    },
    "6" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_workflows_abbreviation(x)
    },
    "7" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_rsample_abbreviation(x)
    },
    NA
  )
}

expand_shiny <- function(x) {
  switch(substr(x, start = 1, stop = 1),
    "1" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
      expand_shiny_default(x)
    },
    NA
  )
}

expand_visualisation <- function(x) {
  switch(substr(x, start = 1, stop = 1),
    "1" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggmisc_abbreviation(x)
    },
    "2" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_cowplot_abbreviation(x)
    },
    "3" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggsci_abbreviation(x)
    },
    "4" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggthemes_abbreviation(x)
    },
    "5" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggforce_abbreviation(x)
    },
    "6" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggridges_abbreviation(x)
    },
    "7" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_ggraph_abbreviation(x)
    },
    "8" = {
      x <- transform_complex_input(x)
      if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
      expand_scales_abbreviation(x)
    },
    NA
  )
}

expand_dev <- function(x) {
  switch(substr(x, start = 1, stop = 1),
    "1" = {
      x <- transform_complex_input(x)
      expand_usethis_abbreviation(x)
    },
    "2" = {
      x <- transform_complex_input(x)
      expand_testthat_abbreviation(x)
    },
    NA
  )
}

# create shortcuts for user defined functions
expand_user_defined <- function(x) {
  user_fun <- utils::lsf.str(envir = .GlobalEnv)
  format <- snakecase::to_snake_case(user_fun)

  test <- lapply(X = strsplit(x = format, split = "_"), function(x) {
    y <- substr(x = x, start = 1, stop = 1)
    y <- c(paste0(y, collapse = ""), paste0(x, collapse = "_"))
  })

  df <- as.data.frame(matrix(unlist(test), ncol = 2, byrow = TRUE))
  df[, 3] <- user_fun
  shortcut <- as.list(df[, 1])
  shortcut <- lapply(shortcut, function(x) {
    rlang::expr_text(x)
  })

  func <- as.list(df[, 2])
  func <- lapply(func, function(x) {
    rlang::expr_text(x)
  })
  # construct arbitrary length case when to check which function the user wants
  input <- dplyr::case_when(
    !!!rlang::parse_exprs(
      paste(
        rlang::expr_text({{ x }}), " == ", shortcut, "~", func
      )
    )
  )
  if (is.na(input)) {
    message(paste("no function with abbreviation", x, "found."))
    return(NULL)
  }
  rstudioapi::insertText(paste0(
    df[, 3][which(df[, 2] == input)],
    "()"
  ))
  reposition(1)
}

# Expansion function if user has changed user preferences for turbo
expand_user_pref <- function(x) {
  y <- unlist(strsplit(x = x, split = "", fixed = T))
  z <- y[1]
  pos_char <-
    lapply(as.list(unlist(list(names(getOption("turbokit-up"))))), function(x) {
      as.character(which(letters == x))
    })
  index <- as.numeric(which(pos_char == z))
  x <- transform_complex_input(x)
  ### TODO:  unname needed?
  expression <- unname(getOption("turbokit-up"))
  dplyr::case_when(
    rlang::parse_expr(paste0(
      z,
      "==",
      index,
      "~",
      deparse(expression[[index]])
    ))
  )
}

turbokit_default <- function(x) {
  switch(x,
    "boot" = boot(),
    "clean" = clean(),
    "read" = {
      rstudioapi::insertText("read()")
      reposition(1)
    },
    "default" = {
      rstudioapi::insertText("default()")
      reposition(1)
    },
    NA
  )
}
