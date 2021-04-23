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
    if (grepl("^[~]{1}", abb, fixed = TRUE)) {
        return(.construct_chain(abb))
    }
    expression <- switch(substr(abb, start = 1, stop = 1),
                         "1" = {
                             abb <- .transform_complex_input(abb)
                             if (!toggle_pipe()$pipe == "+") invisible(toggle())
                             .expand_ggplot_abbreviation(abb)
                         },
                         "2" = {
                             abb <- .transform_complex_input(abb)
                             if (.check_plot_context()) invisible(toggle())
                             .expand_stringr_abbreviation(abb)
                         },
                         "3" = {
                             abb <- .transform_complex_input(abb)
                             if (.check_plot_context()) invisible(toggle())
                             .expand_forcats_abbreviation(abb)
                         },
                         "4" = {
                             abb <- .transform_complex_input(abb)
                             if (.check_plot_context()) invisible(toggle())
                             .expand_clock_abbreviation(abb)
                         },
                         "5" = {
                             abb <- .transform_complex_input(abb)
                             if (!toggle_pipe()$pipe == "%>%") invisible(toggle())
                             .expand_readr_abbreviation(abb)
                         })
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
    chain <- as.list(strsplit(x = input, "(?<=[>])", perl = TRUE)[[1]])
    chain <- lapply(chain, function(x) {
        unlist(strsplit(x = x, "(?=[,>*~])", perl = TRUE))
    })
    chain <- lapply(chain, function(x) {
        unlist(
            lapply(x, FUN = function(y) {
                if (grepl(x = y, pattern = "^[[:digit:]]", perl = TRUE)) {
                    expression <- switch(substr(y, start = 1, stop = 1),
                                         "1" = {
                                             abb <- .transform_complex_input(y)
                                             if (!toggle_pipe()$pipe == "+") invisible(toggle())
                                             .expand_ggplot_abbreviation(abb)
                                         },
                                         "2" = {
                                             abb <- .transform_complex_input(y)
                                             if (.check_plot_context()) invisible(toggle())
                                             .expand_stringr_abbreviation(abb)
                                         },
                                         "3" = {
                                             abb <- .transform_complex_input(y)
                                             if (.check_plot_context()) invisible(toggle())
                                             .expand_forcats_abbreviation(abb)
                                         },
                                         "4" = {
                                             abb <- .transform_complex_input(y)
                                             if (.check_plot_context()) invisible(toggle())
                                             .expand_clock_abbreviation(abb)
                                         },
                                         "5" = {
                                             abb <- .transform_complex_input(y)
                                             if (!toggle_pipe()$pipe == "%>%") invisible(toggle())
                                             .expand_readr_abbreviation(abb)
                                         })
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

    chain <- append(chain, values = list(c("mydata", ">")), after = 0)
    chain[[2]] <- chain[[2]][-1]
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

##########  ggplot2 ##########
#' Function to insert ggplot shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_ggplot <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^g|s|c|f|e|p|t{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter, currently recognized
                    ggplot starting chars:", .ggplot_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (!toggle_pipe()$pipe == "+") {
        # user not in "+" mode and plot / geom / coord / facet / scaleon script line
        if (.check_plot_context()) invisible(toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_ggplot_abbreviation(abb)
    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_ggplot_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 4)
    switch(
        x[1],
        "g" = .expand_ggplot_geom(x),
        "s" = .expand_ggplot_scale(x),
        "c" = .expand_ggplot_coord(x),
        "f" = .expand_ggplot_facet(x),
        "e" = .expand_ggplot_element(x),
        "p" = .expand_ggplot_position(x),
        "t" = .expand_ggplot_theme(x),
        {
            message(paste(
                "first letter:",
                x[1],
                "unknown ggplot abbreviation"))
            NA
        }
    )
}

#' Function to translate abbreviation starting with \code{s} into formatted character string.
#'
#' @param x character vector, maximum length 4.
#' @return full length character string of input.
.expand_ggplot_scale <- function(x) {
    out <- character(length(x))
    out[1] <- "scale"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "x" = "x",
                                        "y" = "y",
                                        "c" = "colour",
                                        "f" = "fill",
                                        "l" = "linetype",
                                        "s" = "size",
                                        "a" = "alpha",
                                        "d" = "discrete",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown ggplot_scale abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    if (length(out) >= 3) {
        out[3] <- sub(x = x[3],
                       pattern = x[3],
                       replacement = .expand_ggplot_scale_tertiary(x[3]),
                       fixed = TRUE)
    }
    if (length(out) == 4) {
        stopifnot(x[4] %in% c("b","c", "d"))
        out[4] <- x[4]
    }
    paste0(out, collapse = "_")
}

#' Function to identify and transform the third element of abbreviation starting with \code{s}.
#'
#' @param y character string of length 1.
#' @return character string of length 1.
.expand_ggplot_scale_tertiary <- function(y) {
    switch(y,
           "b" = "binned",
           "c" = "continuous",
           "d" = "discrete",
           "l" = "log10",
           "r" = "reverse",
           "s" = "sqrt",
           "t" = "time",
           "m" = "manual",
           "g" = "gradient",
           "v" = "viridis",
           "g" = "grey",
           "h" = "hue",
           "f" = "fermenter",
           "i" = "identity",
           {
               message(paste(
                   "third letter:",
                   y,
                   "unknown ggplot_scale abbreviation"))
               NA
           }
    )
}

#' Function to translate abbreviation starting with \code{g} into formatted character string
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input
.expand_ggplot_geom <- function(x) {
    stopifnot(length(x) >= 2 & length(x) < 4)
    out <- character(length(x))
    out[1] <- "geom"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "a" = "abline",
                                        "b" = "boxplot",
                                        "c" = "contour",
                                        "d" = "density_2d",
                                        "e" = "errorbar",
                                        "f" = "freqpoly",
                                        "h" = "histogram",
                                        "j" = "jitter",
                                        "l" = "linerange", #label
                                        "p" = "polygon",
                                        "q" = "quantile",
                                        "r" = "raster",
                                        "s" = "segment", #smooth
                                        "t" = "text",
                                        "v" = "violin",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown ggplot_geom abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    if (length(out) == 3) {
        stopifnot(x[3] == "f")
        out[3] <- "filled"
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{c} into formatted character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_ggplot_coord <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "coord"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   ### TODO: Order of appearance of coord calls
                   replacement = switch(x[2],
                                        "f" = "flip", # fixed?
                                        "s" = "sf",
                                        "c" = "cartesian",
                                        "m" = "map", # munch?
                                        "p" = "polar",
                                        "q" = "quickmap",
                                        "e" = "equal",
                                        "t" = "trans",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown coord abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{f} into formatted character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_ggplot_facet <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "facet"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "w" = "wrap",
                                        "g" = "grid",
                                        "n" = "null",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown facet abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{f} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_ggplot_element <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "element"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "t" = "text",
                                        "b" = "blank",
                                        "l" = "line",
                                        "g" = "grob",
                                        "r" = "rect", # render?
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown element abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{p} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_ggplot_position <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "position"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "d" = "dodge",
                                        "f" = "fill",
                                        "i" = "identity",
                                        "j" = "jitter", # jitterdodge?
                                        "n" = "nudge",
                                        "s" = "stack",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown position abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{t} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_ggplot_theme <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    if(length(out) == 1){
        return("theme")
    }
    out[1] <- "theme"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "m" = "minimal",
                                        "l" = "light",
                                        "b" = "bw",
                                        "v" = "void",
                                        "c" = "classic",
                                        "d" = "dark",
                                        "g" = "gray",
                                        "r" = "replace",
                                        "s" = "set",
                                        "u" = "update",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown theme abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

##########  recipes##########
#' Function to translate recipe package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_recipes <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^r|b|j|p|a|s|c|h|u{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized recipe starting chars:", .recipes_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (!toggle_pipe()$pipe == "%>%") {
        warning("+ operator mode spotted while making recipe.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_recipes_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to transform recipes abbreviation into character string
#'
#' @param x string starting with r.
#' @return Adjusted cursor position in R script
.expand_recipes_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    switch(
        x[1],
        "r" = "recipe", # remove_role?
        "b" = "bake",
        "j" = "juice",
        "p" = "prep",
        "a" = .expand_recipes_all(x), # add?
        "s" = .expand_recipes_step(x),
        "c" = .expand_recipes_check(x),
        "h" = .expand_recipes_has(x),
        "u" = .expand_recipes_update(x),
        {
            message(paste("first letter:",
                          x[1],
                          "unknown recipe abbreviation"))
            NA
        }
    )
}

#' Function to translate recipes abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_recipes_all <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "all"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "n" = "nominal",
                                        "o" = "outcomes",
                                        "p" = "predictors",
                                        "j" = "jitter", # jitterdodge?
                                        "n" = "nudge",
                                        "s" = "stack",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown recipe_all abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
}

#' Function to translate recipes abbreviation starting with s into formatted
#' character string
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_recipes_step <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    out[1] <- "step"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "a" = "arrange",
                                        "b" = "BoxCox", # bagimpute, bin2factor, bs
                                        "c" = "center", # classdist, corr, count, cut
                                        "d" = "dummy", # discretize, downsample
                                        "f" = "filter", # factor2string
                                        "g" = "geodist",
                                        "h" = "holiday", # hyperbolic
                                        "i" = "interact", # intercept, integer, inverse, invlogit, isomap
                                        "k" = "kpca", # knnimpute
                                        "l" = "lincomb", #lag, log, logit
                                        "n" = "normalize",
                                        "o" = "other",
                                        "p" = "pls",
                                        "r" = "regex",
                                        "s" = "scale",
                                        "u" = "unknown",
                                        "w" = "window",
                                        "y" = "YeoJohnson",
                                        "z" = "zv",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown recipe_step abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    if (length(out) == 3){
        if (x[3] == "p" | x[3] == "r") {
            ifelse(x[3] == "p", "poly", "rbf")
        } else if (x[3] == "l") {
            out[3] <- "linear"
            out[2] <- "impute"
        }
    }
    paste0(out, collapse = "_")
}

#' Function to translate recipes abbreviation starting with c into formatted
#' character string
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_recipes_check <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    out[1] <- "check"
    if (length(out) == 3) {
        out[2] <- "new"
        out[3] <- "values"
    } else{
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "class",
                                            "m" = "missing",
                                            "n" = "name",
                                            "r" = "range", # jitterdodge?
                                            "t" = "type",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown recipe_check abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate recipes abbreviation starting with h into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_recipes_has <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "has"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "t" = "type",
                                        "r" = "role",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown recipe_has abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate recipes abbreviation starting with u into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_recipes_update <- function(x){
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "update"
    if (length(out) == 2) {
        out[2] <- "role"
    }
    paste0(out, collapse = "_")
}

##########  parsnip ##########
#' Function to translate parsnip package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script
construct_parsnip <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^d|b|c|e|g{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized parsnip starting chars:", .parsnip_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (!toggle_pipe()$pipe == "%>%") {
        warning("+ operator mode spotted while making recipe.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_parsnip_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to transform recipes abbreviation into character string
#'
#' @param x string starting with p
#' @return Adjusted cursor position in R script
.expand_parsnip_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    out <- character(length(x))
    if (length(out) == 3) {
        ### TODO: what did i want here?
    } else if(length(out) == 2 & x[1] %in% c("d,b,c,e")) {
        switch(
            x[1],
            "d" = "decision_tree", # remove_role?
            "b" = "boost_tree",
            "c" = "C5.0_train",
            "e" = "eval_args",
            "g" = .expand_parsnip_get(x), # add?
            {
                message(paste(
                    "first letter:",
                    x[1],
                    "unknown parsnip abbreviation"))
                NA
            }
        )
    } else {

    }
    switch(
        x[1],
        "r" = "recipe", # remove_role?
        "b" = "bake",
        "j" = "juice",
        "p" = "prep",
        "a" = .expand_recipes_all(x), # add?
        "s" = .expand_recipes_step(x),
        "c" = .expand_recipes_check(x),
        "h" = .expand_recipes_has(x),
        "u" = .expand_recipes_update(x),
        {
            message(paste(
                "first letter:",
                x[1],
                "unknown parsnip abbreviation"))
            NA
        }
    )
}

#' Function to translate parsnip abbreviation starting with \code{a} into
#' formatted character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_parsnip_get <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "get"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "d" = "dependency",
                                        "e" = "encoding",
                                        "m" = "model",
                                        "j" = "jitter", # jitterdodge?
                                        "n" = "nudge",
                                        "s" = "stack",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[1],
                                                "unknown parsnip_get abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
}
### TODO: Needs thorough check for naming policy

##########  stringr ##########
#' Function to insert stringr shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_stringr <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^s|w{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized stringr starting chars:", .stringr_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (.check_plot_context()) {
        warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_stringr_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with s
#' @return Adjusted cursor position in R script
.expand_stringr_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    switch(
        x[1],
        "s" = .expand_stringr_str(x),
        "w" = "word",
        {
            message(paste(
                "first letter:",
                x[1],
                "unknown stringr abbreviation"))
            NA
        }
    )
}

#' Function to translate abbreviation starting with \code{s} into formatted
#' character string.
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_stringr_str <- function(x) {
    out <- character(length(x))
    out[1] <- "str"
    if (length(out) == 3) {
        out[3] <- sub(x = x[3],
                       pattern = x[3],
                       replacement = switch(x[3],
                                            "a" = "all",
                                            "d" = "data",
                                            "l" = "lower",
                                            "u" = "upper",
                                            "t" = "title",
                                            "f" = "fixed",
                                            {
                                                message(paste(
                                                    "third letter:",
                                                    x[3],
                                                    "unknown stringr abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "r" = "remove", # replace
                                            "e" = "extract",
                                            "m" = "match",
                                            "d" = "data",
                                            "l" = "locate",
                                            "g" = "data",
                                            "t" = "to",
                                            "s" = "split",
                                            "v" = "view",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown stringr abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "l" = "length",
                                            "c" = "count",
                                            "d" = "detect",
                                            "e" = "extract",
                                            "f" = "flatten",
                                            "g" = "glue",
                                            "m" = "match",
                                            "o" = "order",
                                            "p" = "pad",
                                            "r" = "remove", # replace
                                            "s" = "split", #subset
                                            "t" = "trim",
                                            "v" = "view",
                                            "w" = "wrap",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[1],
                                                    "unknown stringr abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

##########  clock ##########
#' Function to insert clock shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_clock <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^a|d|g|i|s|w|z{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized clock starting chars:", .clock_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (.check_plot_context()) {
        warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_clock_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to construct abbreviation into character string
#'
#' @param x string starting with c
#' @return Adjusted cursor position in R script
.expand_clock_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 5)
    switch(
        x[1],
        "a" = .expand_clock_add(x), # as
        "d" = .expand_clock_date(x),
        "g" = .expand_clock_get(x),
        "i" = .expand_clock_is(x),
        # "n" = "naive"
        "s" = .expand_clock_set(x),
        "w" = .expand_clock_weekday(x),
        "z" = .expand_clock_zone(x),
        {
            message(paste(
                "first letter:",
                x[1],
                "unknown clock abbreviation"))
            NA
        }
    )
}

#' Function to translate abbreviation starting with \code{a} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_clock_add <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "add"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "d" = "day",
                                        "h" = "hours",
                                        "m" = "minutes", # months, microseconds, miliseconds
                                        "n" = "nanoseconds",
                                        "q" = "quarters",
                                        "s" = "seconds",
                                        "w" = "weeks",
                                        "y" = "years",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown clock_add abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{d} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_clock_date <- function(x) {
    stopifnot(length(x) <= 4)
    out <- character(length(x))
    out[1] <- "date"
    if (length(out) == 3) {
        out[3] <- sub(x = x[3],
                       pattern = x[3],
                       replacement = switch(x[3],
                                            "y" = "year",
                                            "z" = "zone",
                                            "f" = "factor", #floor
                                            "p" = "parse",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[1],
                                                    "unknown clock_date abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
        out[2] <- sub(x = x[3],
                       pattern = x[3],
                       replacement = switch(x[3],
                                            "l" = "leap",
                                            "m" = "month",
                                            "s" = "set", #floor
                                            "w" = "weekday",
                                            {
                                                message(paste(
                                                    "third letter:",
                                                    x[1],
                                                    "unknown clock_date abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "b" = "build",
                                            "c" = "ceiling",
                                            "f" = "format", #floor
                                            "g" = "group",
                                            "p" = "parse",
                                            "r" = "round",
                                            "s" = "shift",
                                            "z" = "zone",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[1],
                                                    "unknown clock_date abbreviation"))
                                                NA
                                            }

                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{g} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_clock_get <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "get"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "y" = "year",
                                        "m" = "minute", # months, microseconds, miliseconds
                                        "d" = "day",
                                        "w" = "week",
                                        "i" = "index",
                                        "q" = "quarter",
                                        "s" = "second",
                                        "n" = "nanosecond",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[1],
                                                "unknown clock_get abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{i} into formatted
#' character string.
#'
#' @param x character vector, maximum length 5.
#' @return full length character string of input.
.expand_clock_is <- function(x) {
    stopifnot(length(x) <= 5)
    out <- character(length(x))
    out[1] <- "is"
    if (length(out) == 5) {
        out <- "is_iso_year_week_day"
    } else if (length(out) == 4) {
        out[2] <- "year"
        out[3] <- sub(x = x[3],
                       pattern = x[3],
                       replacement = switch(x[3],
                                            "m" = "month",
                                            "q" = "quarter",
                                            "w" = "week",
                                            {
                                                message(paste(
                                                    "third letter:",
                                                    x[3],
                                                    "unknown clock_is abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
        out[4] <- sub(x = x[4],
                       pattern = x[4],
                       replacement = switch(x[4],
                                            "d" = "day",
                                            "w" = "weekday",
                                            {
                                                message(paste(
                                                    "fourth letter:",
                                                    x[4],
                                                    "unknown clock_is abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else if (length(out) == 3) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "y" = "year",
                                            "n" = "naive",
                                            "s" = "sys",
                                            "z" = "zoned",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown clock_is abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "d" = "duration",
                                            "w" = "weekday",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown clock_is abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{s} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_clock_set <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "set"
    out[2] <-  sub(x = x[2],
                    pattern = x[2],
                    replacement = switch(x[2],
                                         "y" = "year",
                                         "m" = "minute", # months, microseconds, miliseconds
                                         "d" = "day",
                                         "w" = "week",
                                         "i" = "index",
                                         "q" = "quarter",
                                         "s" = "second",
                                         "n" = "nanosecond",
                                         {
                                             message(paste(
                                                 "second letter:",
                                                 x[1],
                                                 "unknown clock_set abbreviation"))
                                             NA
                                         }
                    ),
                    fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{w} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_clock_weekday <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "weekday"
    if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "code",
                                            "f" = "factor",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown clock_set abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{z} into formatted
#' character string.
#'
#' @param x character vector, maximum length 4.
#' @return full length character string of input.
.expand_clock_zone <- function(x) {
    stopifnot(length(x) <= 4)
    out <- character(length(x))
    out[1] <- ifelse(x[2] == "d", "zone", "zoned")
    out[2] <- ifelse(x[2] == "d", "database", "time")
    if (length(out) == 4) {
        out[3] <- "parse"
        out[4] <- ifelse(x[4] == "a", "abbrev", "complete")
    } else if (length(out) == 3) {
        if (out[1] == "zone") {
            out[3] <- ifelse(x[3] == "n", "names", "version")
        } else {
            out[3] <- sub(x = x[3],
                           pattern = x[3],
                           replacement = switch(x[3],
                                                "n" = "now",
                                                "p" = "precision",
                                                "z" = "zone",
                                                {
                                                    message(paste(
                                                        "third letter:",
                                                        x[1],
                                                        "unknown clock_zone abbreviation"))
                                                    NA
                                                }
                           ),
                           fixed = TRUE)
        }
    }
    paste0(out, collapse = "_")
}

##########  forcats ##########
#' Function to insert forcats shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_forcats <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^d|a|l{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized forcats starting chars:", .forcats_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (.check_plot_context()) {
        warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
        invisible(turbokit::toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

    expression <- .expand_forcats_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}


#' Function to construct abbreviation into character string
#'
#' @param x string starting with f
#' @return Adjusted cursor position in R script
.expand_forcats_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 5)
    switch(
        x[1],
        "f" = .expand_forcats_fct(x),
        "a" = "as_factor", # as
        "l" = .expand_forcats_lvls(x),
        {
            message(paste("first letter:", x[1], "unknown forcats abbreviation"))
            NA
        }
    )
}

#' Function to translate abbreviation starting with \code{a} into formatted
#' character string.
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_forcats_fct <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    out[1] <- "fct"
    if (length(out) == 3) {
        out[3] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "p" = "prop",
                                            "l" = "lowfreq",
                                            "m" = "min",
                                            "n" = "n",
                                            {
                                                message(paste(
                                                    "third letter:",
                                                    x[1],
                                                    "unknown forcats_lump abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
        out[2] <- "lump"
        if (x[2] == "e") {
            out[2] <- "explicit"
            out[3] <- "na"
        }
    } else if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "r" = "reorder", #rev, relabel, recode, relevel
                                            "l" = "lump",
                                            "d" = "drop", # months, microseconds, miliseconds
                                            "i" = "infreq", #inorder #inseq
                                            "e" = "expand",
                                            "a" = "anon",
                                            "c" = "collapse", # count, cross
                                            "m" = "match",
                                            "o" = "other",
                                            "s" = "shuffle", # shift
                                            "u" = "unique", # unify
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown forcats_fct abbreviation"))
                                                NA
                                            }

                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{a} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_forcats_lvls <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "lvls" # if
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "r" = "reorder",
                                        "e" = "expand",
                                        "u" = "union",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown forcats_lvls abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}


##########  readr ##########
#' Function to insert readr shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_readr <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    ### TODO:  if nchar is 1, make shortcut of widely used functions
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^r|w|t|c|d|f|m|p|s{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized readr starting chars:", .readr_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    mode <- ifelse(toggle_pipe()$pipe == "%>%", 1, 0)
    if (!mode == 1) {
        warning("+ operator mode spotted while plotting.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
        invisible(turbokit::toggle())
    }

    ### TODO: allow for numbers? multiple non-unique letter combi
    abb <- tolower(unlist(strsplit(input, split = "", fixed = TRUE)))

    expression <- .expand_readr_abbreviation(abb)

    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to construct abbreviation into character string
#'
#' @param x string starting with r
#' @return Adjusted cursor position in R script
.expand_readr_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    switch(
        x[1],
        "r" = .expand_readr_read(x),
        "w" = .expand_readr_write(x),
        "t" = .expand_readr_tokenize(x),
        "c" = .expand_readr_col(x),
        "d" = .expand_readr_date(x),
        "f" = .expand_readr_format(x),
        "m" = .expand_readr_melt(x),
        "p" = .expand_readr_parse(x),
        "s" = .expand_readr_spec(x),
        {
            message(paste("second letter:", x[2], "unknown readr abbreviation"))
            NA
        }
    )
}

#' Function to translate abbreviation starting with \code{r} into formatted
#' character string.
#'
#' @param x character vector, maximum length 4.
#' @return full length character string of input.
.expand_readr_read <- function(x) {
    stopifnot(length(x) <= 4)
    out <- character(length(x))
    out[1] <- "read"
    if (length(out) == 4) {
        out[2] <- "lines"
        out[3] <- "raw"
        out[4] <- "chunked"
    } else if (length(out) == 3) {
        out[3] <- ifelse(x[3] == "c", "chunked", "raw")
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv", #csv2
                                            "l" = "lines",
                                            "d" = "delim",
                                            "t" = "tsv",
                                            "f" = "file",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_read abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv",
                                            "t" = "table", #tsv
                                            "l" = "lines", #log
                                            "d" = "delim",
                                            "f" = "file", #fwf
                                            "r" = "rds",
                                            "b" = "builtin", #csv2
                                            "e" = "example",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_read abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{w} into formatted
#' character string.
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_readr_write<- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    out[1] <- "write"
    if (length(out) == 3) {
        out[3] <- "csv"
        out[2] <- "excel"
    } else if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv",
                                            "d" = "delim",
                                            "f" = "file",
                                            "l" = "lines",
                                            "r" = "rds",
                                            "t" = "tsv",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_write abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{t} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_tokenize <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "tokenize"
    if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv",
                                            "d" = "delim",
                                            "f" = "fwf",
                                            "l" = "line",
                                            "t" = "tsv",
                                            "w" = "ws",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_tokenize abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{c} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_col <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "col"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "c" = "character",
                                        "d" = "date", # datetime double
                                        "f" = "factor",
                                        "g" = "guess",
                                        "i" = "integer",
                                        "l" = "logical",
                                        "n" = "number",
                                        "s" = "skip",
                                        "t" = "time",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown readr_col abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{d} into formatted
#' character string.
#'
#' @param x character vector, maximum length 3.
#' @return full length character string of input.
.expand_readr_date <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    out <- ifelse(length(out) == 3, "date_names_lang", "date_names")
}

#' Function to translate abbreviation starting with \code{f} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_format <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "format"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "c" = "csv",
                                        "d" = "delim",
                                        "t" = "tsv",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown readr_format abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{m} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_melt <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "melt"
    if (length(out) == 3) {
        out[3] <- "chunked"
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv",
                                            "d" = "delim",
                                            "t" = "tsv",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_melt abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    } else if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                       pattern = x[2],
                       replacement = switch(x[2],
                                            "c" = "csv",
                                            "d" = "delim",
                                            "t" = "tsv", # table
                                            "f" = "fwf",
                                            {
                                                message(paste(
                                                    "second letter:",
                                                    x[2],
                                                    "unknown readr_melt abbreviation"))
                                                NA
                                            }
                       ),
                       fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{p} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_parse <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "parse"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "c" = "character",
                                        "d" = "date", #date time double
                                        "f" = "factor",
                                        "g" = "guess",
                                        "i" = "integer",
                                        "l" = "logical",
                                        "n" = "number",
                                        "t" = "time",
                                        "v" = "vector",
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown readr_parse abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}

#' Function to translate abbreviation starting with \code{p} into formatted
#' character string.
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_readr_spec <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "spec"
    out[2] <- sub(x = x[2],
                   pattern = x[2],
                   replacement = switch(x[2],
                                        "c" = "csv",
                                        "d" = "delim",
                                        "t" = "tsv", # table
                                        {
                                            message(paste(
                                                "second letter:",
                                                x[2],
                                                "unknown readr_spec abbreviation"))
                                            NA
                                        }
                   ),
                   fixed = TRUE)
    paste0(out, collapse = "_")
}
