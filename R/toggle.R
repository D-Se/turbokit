shortcut_options <- settings::options_manager(pipe = "%>%",
                                              .allowed = list(
                                                  pipe = settings::inlist("%>%", "+")
                                              ))

#' An S4 class for storing toggle options
#'
#' @slot options function to get and set options
#' @slot value character vector describing operator type
PipeClass <- setClass("PipeClass",
                      slots = list(options = 'function',
                                   value = 'character'),
                      prototype = list(
                          options = shortcut_options,
                          value = "%>%"
                      )
)

setGeneric("toggle_pipe", function(where = NULL, ...) standardGeneric("toggle_pipe"))

#' Function to get and set pipe operators
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("toggle_pipe", "ANY", function(where = NULL, ...) {
    do.call(shortcut_options, c(where, list(...)))
})

#' Function to get and set pipe operator in options
#'
#' @param where place to insert toggle
#' @param ... passed down to construct list
#' @return pipe operator options
setMethod("toggle_pipe", "PipeClass", function(where = NULL, ...) {
    if (settings::is_setting(...)){
        where@options <- settings::clone_and_merge(where@options, ...)
    } else {
        where@options(...)
    }
})

#' Get and set pipe operators in package options.
#'
#' @return Changing pipe operator settings in package options between \code{\%>\%} and \code{+}.
#' @export
toggle <- function() {
    get_current <- toggle_pipe()$pipe
    ifelse(get_current == "%>%", toggle_pipe(pipe = "+"), toggle_pipe(pipe = "%>%"))
}
