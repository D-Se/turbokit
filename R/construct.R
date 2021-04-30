### TODO: parameter expand functions

#' Wrapper constructor function to insert tidyverse functions at the cursor position
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{construct_complex()} is a wrapper for \code{construct_*} functions. It
#' allows for rapid insertion of many popular functions living in
#' different packages in a shorthand style, reducing time spent inputting and
#' formatting code.
#'
#' It has powerful side effect, which is its formula-like \emph{tidymath}
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
construct_complex <- function(input = NULL) {
    mode <- mode_toggle()$mode
    abb <- character(length = 1)
    if (is.null(input)) {
        abb <- .get_complex_userinput()
        if (abb == "error" | abb == "warning") {
            return(NULL) # avoid stop() nasty addin error pop-up box
        }
    } else {
        if (!is.character(input)){
            message("coercing input to character")
            input <- as.character(input)
        }
        abb <- input
    }
    # translate tidymath
    if (grepl(x = abb, pattern = "^[~]{1}", perl = TRUE)) {
        return(.construct_chain(abb))
        # default abbreviation per mode
    } else if (grepl(x = abb, pattern = "^[[:alpha:]]", perl = TRUE)) {
        expression <- .expand_default_abbreviation(abb)
    } else if (grepl(x = abb, pattern = "^[0]{1}", perl = TRUE)){
        abb <- sub(x = abb, pattern = ".", replacement = "", perl = TRUE)
        return(.expand_user_defined(abb))
    } else {
        if (mode == "tidyverse") {
            expression <- .expand_tidyverse(abb)
        } else if (mode == "tidymodels") {
            expression <- .expand_tidymodels(abb)
        } else if (mode == "shiny") {
            expression <- .expand_shiny(abb)
        }
    }
    if (grepl("NA", expression, perl = T)) {
        message("Unanticipated verb - if issue persist, please file a bug report.")
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

# stringi::stri_detect_fixed(out[i], "0")
#' Translates tidymath into snippet format.
#'
#' @param input a list of tidyverse verbs, each list element is a row in output
#' @return a new entry in r.snippets fall called "s"
.construct_snippet <- function(input) {
    ### TODO:vectorised stri_replace_all_fixed here
    out <- stringr::str_replace_all(string = input, pattern = .NAMED_PATTERNS)
    m <- 1
    # irretrievable object from here, syntax starts with $
    for (i in seq_along(out)) {
        num <- sum(stringi::stri_count(out[i], fixed = "0"))
        # if multiple snippet positions detected - split into subroutine
        if (num > 1) {
            temp <- unlist(stringi::stri_split(str = out[i], regex = "(?<=[=])"))
            build <- character(length = length(temp))
            for (j in seq_along(temp)) {
                inter <- temp[j]
                build[j] <- sub(x = inter,
                                pattern = "0",
                                replacement = as.character(m))
                m <- m + 1
            }
            out[i] <- stringi::stri_c(build, collapse = "")
        } else {
            out[i] <- stringi::stri_replace_all_fixed(str = out[i],
                                                      pattern = "0",
                                                      replacement = as.character(m))
            m <- m + 1
        }
    }
    .add_snippet(name = "s",
                        body = out)
}

#' Transformation step of dialog box input to be construct_* function-friendly
#'
#' @param input character string to transform to snippet syntax
#' @return character vector stripped of any numeric indicator
.construct_chain <- function(input) {
    mode <- mode_toggle()$mode
    chain <- as.list(strsplit(x = input, "(?<=[>])", perl = TRUE)[[1]])
    chain <- lapply(chain, function(x) {
        unlist(strsplit(x = x, "(?=[,>*~])", perl = TRUE))
    })
    chain <- lapply(chain, function(x) {
        unlist(
            lapply(x, FUN = function(y) {
                if (grepl(x = y, pattern = "^[[:digit:]]", perl = TRUE)) {
                    if (mode == "tidyverse") {
                        expression <- .expand_tidyverse(y)
                    } else if (mode == "tidymodels") {
                        expression <- .expand_tidymodels(y)
                    } else if (mode == "shiny") {
                        expression <- .expand_shiny(y)
                    }
                    paste0(expression, "()")
                } else if (nchar(y) > 1) {
                    expression <- switch(y,
                                         "sw" = "starts_with",
                                         "ew" = "ends_with",
                                         "rj" = "right_join",
                                         "lj" = "left_join",
                                         "cw" = "case_when",
                                         NA)
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
                                         NA)
                    if (startsWith(expression, ",") | startsWith(expression,">")
                        | startsWith(expression, "*") | startsWith(expression, "~")) {
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
        if (any(grepl("[*]", x =  x, perl = TRUE))) {
            x <- .transform_complex_interaction(x)
        }
        sub(x = x, pattern = ">", replacement = "%>%", fixed = TRUE)
    })
    chain <- lapply(lapply(chain[!!lengths(chain)], toString),
                    function(x) { sub(x = x, pattern = ",",
                                     replacement = "",
                                     fixed = TRUE)})
    chain <- lapply(chain, function(x) {
        if(any(grepl(x = x,
                     pattern = "ggplot|geom|facet|position|scale|coord|element|theme",
                     perl = TRUE))){
            x <- sub(x = x, pattern = "%>%",
                     replacement = "+",
                     fixed = TRUE)
            x <- sub(x = x, pattern = "ggplot\\(\\)",
                     replacement = "ggplot(aes())",
                     perl = TRUE)
        } else
            x
    })
    # remove any operator left at the end
    chain[[length(chain)]] <- sub(chain[[length(chain)]],
                                  pattern = "\\+|\\%>\\%",
                                  replacement = "",
                                  perl = TRUE)
    styler::style_text(sapply(chain[!!lengths(chain)], toString))
}


#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_tidyverse <- function(x){
    switch(substr(x, start = 1, stop = 1),
           "1" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
               .expand_ggplot_abbreviation(x)
           },
           "2" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_stringr_abbreviation(x)
           },
           "3" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_forcats_abbreviation(x)
           },
           "4" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_clock_abbreviation(x)
           },
           "5" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
               .expand_readr_abbreviation(x)
           },
           NA)
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_tidymodels <- function(x){
    switch(substr(x, start = 1, stop = 1),
           "1" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "+") invisible(toggle_pipe())
               .expand_recipes_abbreviation(x)
           },
           "2" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_parsnip_abbreviation(x)
           },
           "3" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_tune_abbreviation(x)
           },
           "4" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_dials_abbreviation(x)
           },
           "5" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
               .expand_yardstick_abbreviation(x)
           },
           "6" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
               .expand_workflows_abbreviation(abb)
           },
           "7" = {
               x <- .transform_complex_input(x)
               if (!pipe_toggle()$pipe == "%>%") invisible(toggle_pipe())
               .expand_rsample_abbreviation(x)
           },
           NA)
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_shiny <- function(x){
    switch(substr(x, start = 1, stop = 1),
           "1" = {
               x <- .transform_complex_input(x)
               if (.check_plot_context()) invisible(toggle_pipe())
               .expand_shiny_abbreviation(x)
           },
           NA)
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_default_abbreviation <- function(x){
    if (nchar(x) == 2) {
        expression <- switch(substr(x, start = 1, stop = 2),
                             "sw" = "starts_with",
                             "ew" = "ends_with",
                             "rj" = "right_join",
                             "lj" = "left_join",
                             "cw" = "case_when",
                             "ao" = "any_of",
                             "nr" = "num_range",
                             {
                                 message(
                                     "No default word found. Did you forget a leading digit?")
                                 NA
                             })
    } else if (nchar(x == 1)) {
        expression <- switch(substr(x, start = 1, stop = 1),
                             "m" = "mutate",
                             "f" = "filter",
                             "s" = "select",
                             "p" = "ggplot",
                             "z" = "summarise",
                             "a" = "across",
                             "e" = "everything",
                             "g" = "group_by",
                             "c" = "contains",
                             {
                                 message(
                                     "No default word found. Did you forget a leading digit?")
                                 NA
                             })
    }
}


#' Translates and inserts abbreviation of function in user's global environment
#'
#' @param x an abbreviation
#' @return a new entry in r.snippets fall called "s"
#' @importFrom utils lsf.str
.expand_user_defined <- function(x){
    user_fun <- lsf.str(envir = .GlobalEnv)
    format <- snakecase::to_snake_case(user_fun)

    test <- lapply(X = strsplit(x = format, split = "_"), function(x){
        y <- substr(x = x, start = 1, stop = 1)
        y <- c(paste0(y, collapse = ""), paste0(x, collapse = "_"))
    })

    df <- as.data.frame(matrix(unlist(test), ncol = 2, byrow = TRUE))
    df[,3] <- user_fun
    shortcut <- as.list(df[,1])
    shortcut <- lapply(shortcut, function(x){
        rlang::expr_text(x)
    })

    func <- as.list(df[,2])
    func <- lapply(func, function(x){
        rlang::expr_text(x)
    })
    # construct arbitrary length case when to check which function the user wants
    input <- dplyr::case_when(
        !!!rlang::parse_exprs(
            paste(
                rlang::expr_text({{ x }}), " == ", shortcut, "~", func)))
    if (is.na(input)) {
        message(paste("no function with abbreviation", x, "found."))
        return(NULL)
    }
    rstudioapi::insertText(paste0(
        df[,3][which(df[,2] == input)],
        "()"
    ))
    .reposition(1)
}
