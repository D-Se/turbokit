pipe_opts <- settings::options_manager(
    pipe = "%>%",
    .allowed = list(
        pipe = settings::inlist("%>%", "+")
    ))
mode_opts <- settings::options_manager(
    mode = "tidyverse",
    .allowed = list(
        mode = settings::inlist("tidyverse",
                                "visualisation",
                                "tidymodels",
                                "shiny",
                                # possible but not visible (few users use this)
                                "dev")
    ))

#' An S4 class for storing turbokit toggle options
#'
#' @slot pipe_options function to get and set options
#' @slot mode_options function to get and set package shortcut mode
#' @slot value character vector describing operator type
Turbo <- setClass("Turbo",
                      slots = list(pipe_options = 'function',
                                   pipe_value = 'character',
                                   mode_options = 'function',
                                   mode_value = 'character'
                                   ),
                      prototype = list(
                          pipe_options = pipe_opts,
                          pipe_value = "%>%",
                          mode_options = mode_opts,
                          mode_value = "tidyverse"
                      )
)

setGeneric("pipe_toggle", function(where = NULL, ...) standardGeneric("pipe_toggle"))
setGeneric("mode_toggle", function(where = NULL, ...) standardGeneric("mode_toggle"))

#' Function to get and set pipe operators
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("pipe_toggle", "ANY", function(where = NULL, ...) {
    do.call(pipe_opts, c(where, list(...)))
})

#' Function to get and set pipe operator in options
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("pipe_toggle", "Turbo", function(where = NULL, ...) {
    if (settings::is_setting(...)){
        where@pipe <- settings::clone_and_merge(where@pipe, ...)
    } else {
        where@pipe(...)
    }
})

#' Function to get and set pipe operators
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("mode_toggle", "ANY", function(where = NULL, ...) {
    do.call(mode_opts, c(where, list(...)))
})

#' Function to get and set pipe operator in options
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("mode_toggle", "Turbo", function(where = NULL, ...) {
    if (settings::is_setting(...)){
        where@mode <- settings::clone_and_merge(where@mode, ...)
    } else {
        where@mode(...)
    }
})

#' Get and set pipe operators in package options.
#'
#' @return Changing pipe operator settings in package options between \code{\%>\%} and \code{+}.
#' @export
toggle_pipe <- function() {
    get_current <- pipe_toggle()$pipe
    ifelse(get_current == "%>%", pipe_toggle(pipe = "+"), pipe_toggle(pipe = "%>%"))
}

#' Get and set turbokit complex input modes
#'
#' @param mode_opt specification of mode by user
#' @return Changing mode settings in package options between \code{tidyverse}, \code{tidymodels}, and \code{shiny}.
#' @export
toggle_mode <- function(mode_opt = NULL) {
    new <- character(length = 1)
    get_current <- mode_toggle()$mode
    if (get_current == "dev") {
        if (getOption("turbokit-verbose")) {
            cat(paste0("Turbokit mode: " , mode_toggle()$mode, "\n"))
        }
        return(mode_toggle(mode = "tidyverse"))
    }
    if (is.null(mode_opt)) {
        new <- dplyr::case_when(get_current == "tidyverse" ~ "visualisation",
                                get_current == "visualisation" ~ "tidymodels",
                                get_current == "tidymodels" ~ "shiny",
                                get_current == "shiny" ~ "tidyverse")
        if (getOption("turbokit-verbose")) {
            cat(paste0("Turbokit mode: " , new, "\n"))
        }
        return(mode_toggle(mode = new))
    } else if (is.character(mode_opt)) {
        mode_opt <- tolower(mode_opt)
        # verse model shiny
        if (mode_opt %in% c("tv", "v", "tm", "s", "d")) {
            mode_opt <- dplyr::case_when(mode_opt == "tv" ~ "tidyverse",
                                         mode_opt == "v" ~ "visualisation",
                                         mode_opt == "tm" ~ "tidymodels",
                                         mode_opt == "s" ~ "shiny",
                                         mode_opt == "d" ~ "dev")
        }
        stopifnot(mode_opt %in% c("tidyverse", "tidymodels",
                                  "shiny", "visualisation", "dev"))
        new <- mode_opt
    } else if (is.numeric(mode_opt)) {
        # currently only 3 modes, may expand in future
        stopifnot(mode_opt <= 3)
        new <- dplyr::case_when(mode_opt == 1 ~ "tidyverse",
                                mode_opt == 2 ~ "visualisation",
                                mode_opt == 3 ~ "tidymodels",
                                mode_opt == 4 ~ "shiny")

    }
    if (get_current == new ) {
        message(paste("already in", new, "mode"))
        return(invisible(NULL))
    }
    mode_toggle(mode = new)
    if (getOption("turbokit-verbose")) {
        cat(paste0("Turbokit mode: " , mode_toggle()$mode, "\n"))
    }
}

