##########  Operators ##########
#' Function to create operator that mimics an unary operator
#' Not meant to be called directly.
#'
#' @param name name of function to construct
#' @param body body of the function of operator to create
#'
#' @return seemingly unary infix function
`%.%` <- function(name, body) {
  `%paste%` <- paste0
  fun_name <- "%" %paste% name %paste% "%"

  assign(
    x = fun_name,
    value = function(x, comment) body(x),
    envir = parent.frame()
  )
}

#' Superpipe operator: translate tidymath to dynamic snippets.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Executes translation of tidymath, construction of dynamic snippet and inserts
#' the snippet at the cursor location. Designed to called by shortcut.
#'
#' @param x a character string in tidymath format to convert
#' @param comment quasi-comment
#' @return A dynamic snippet added to R.snippets file
#'
#' @section Superpipe use:
#'
#' \bold{The superpipe}
#'
#' The operator translates a character string of \emph{tidymath} at the current cursor
#' location to a \emph{dynamic snippet}. It is available as an add-in shortcut,
#' which is where its utility and powers shine. The superpipe operator is not
#' designed to be typed by hand, as at its core, the \emph{turbokit} package
#' seeks to reduce button presses for frequently used functions and code formatting.
#' Without using the shortcut, the user must add \bold{something} of
#' length 1, after the operator.
#' The superpipe operator shortcut is available after package installation, but
#' unlike the regular pipe operator shortcut, is not able to be called executed
#' wthout loading the \emph{turbokit} package, for user safety reasons.
#'
#' \bold{Dynamic snippets}
#'
#' When called using its shortcut format, the operator translates a character
#' string of \emph{tidymath} at the current cursor location to a \emph{dynamic snippet}.
#' A feature of the snippet, a text macro, is that users can press TAB to move to
#' specified locations. Dynamic snippets differ from regular snippets in that
#' input locations may vary depending on 1) the user input in the script, and
#' 2) superpipe-generated locations. The superpipe operator can be used to
#' avoid the quirks and alien syntax of the snippet system while still enjoying
#' its benefits. The superpipe includes a guessing mechanism that, if no locations
#' are specified by the user, will insert locations based on common use cases.
#'
#' The dynamic snippets are synergistic with \code{turbbokit}'s \code{construct_complex}
#' function, which is the translation engine that converts \emph{tidymath}. This
#' means that while TABbing through inputs, the user can call the \code{construct_complex}
#' shortcut to insert function calls, without breaking the snippet.
#'
#' The superpipe operator cleans up after itself. While the snippets remain available
#' until the snippet page is refreshed by restarting the R session or manually
#' opening and saving the snippet page, the superpipe-created snippet \emph{s}
#' is removed from the R.snippets file.
#'
#' \bold{quasi-commenting}
#'
#' The seemingly arbitrary input to be inserted after the superpipe operator is
#' there to satisfy a constraint to user-defined operators, which may not be unary.
#' A side effect is that the input is ignored entirely and has no effect on the
#' workings of the superpipe. As such, this piece of input can serve as a
#' nicely specially formatted comment, here called quasi-comment, which is not
#' evaluated yet syntax highlighted as if it were normal code.
#'
#' Without the shortcut, the user can manually type \bold{almost any} input
#' of length 1 after the operator. Bar keywords like \code{for} or \code{in} and
#' the exclamation mark, any input is viable if approached cleverly.
#' This behavior extends to markdown. Note - it could lead to unintended behavior
#'
#' @export
`%>>%` <- ">>" %.% function(x) {
  out <- construct_chain(x)
  # snippet gets added here
  out <- construct_snippet(out)
  s <- {
    suppressMessages(usethis::edit_rstudio_snippets(type = "r"))
    rstudioapi::documentSave()
    rstudioapi::documentClose()
    remove_snippet(
      name = "s",
      path = getOption("turbokit-snippetdir")
    )
    reposition_row()
    {
      rstudioapi::insertText("s")
      if (Sys.info()["sysname"] == "Windows" &
        getOption("turbokit-autoinsert")) {
        ### TODO: access shortcuts and default shortcuts in R.
        # mimic insert snippet code // programmatically insert hook.
        if (requireNamespace("KeyboardSimulator", quietly = TRUE)) {
          KeyboardSimulator::keybd.press(button = "shift+tab")
        }
      }
    }
  }
}

########## Insert operators ##########

#' Instigate dynamic complex pipe snippet processing and insertion.
#'
#' @return \code{\%>\%.} inserted at the current cursor location and a custom
#' snippet object \code{s} in the next row.
#'
#' @section Insert superpipe:
#'
#' The superpipe is the entry point for translation of tidymath. It is designed
#' to be called via shortcut, and is primarily called for its side effects.
#'
#' \code{superpipe()} is a wrapper function that initiates a series of turbokit
#' function calls to translate tidymath into a dynamic snippet, and helps the
#' user to insert the newly created snippet directly into the R script.
#'
#' Its side effects, including code formatting and auto-insertion are tweakable
#' through package options. Some of these options are operating system-specific.
#' See \code{vignette("Turbokit")} for available options and adjusting superpipe
#' output to your preference.
#'
#' \bold{Formatting}
#' There are some limitations to automated formatting.
#' 1.
#' multi-line tidymath statements require the superpipe operator \code{\%>>\%} to
#' be manually typed, and an arbitrary symbol to be inserted after ward like so
#' \code{\%>>\%.}.
#'
#' @export
superpipe <- function() {
  reposition_end()
  rstudioapi::insertText("%>>%.")
  if (!"turbokit" %in% .packages()) {
    library(turbokit)
  }
  x <- rstudioapi::getActiveDocumentContext()
  y <- rstudioapi::primary_selection(x = x)
  if (stringi::stri_detect_regex(
    str = x$contents[y$range$end[1]],
    pattern = "[[:lower:]]",
    max_count = 1
  )) {
    rstudioapi::insertText("\n")
  } else if (y$range$end[1] == length(x$contents)) {
    rstudioapi::insertText("\n")
  }
  z <- x$contents[y$range$start[1]]
  rstudioapi::sendToConsole(z, execute = T, focus = F)
}

# code adapted from mufflr package (performance, generalized, extended)
# original author Miles McBain, under MIT license.

#' Insert indented smart pipe operator in R script.
#'
#' @return \code{\%>\%} or \code{+} in an R script at cursor location,
#' depending on toggle option, with indenting applied to user settings.
#'
#' @export
insert_pipe <- function() {
  pipe <- pipe_toggle()$pipe
  x <- rstudioapi::getActiveDocumentContext()
  y <- x$selection[[1]]$range
  row <- y$end["row"]
  col <- y$end["column"]
  n_nested <- sum(gregexpr(
    pattern = "[)\"]",
    text = x$contents[row],
    perl = TRUE
  )[[1]] > 0)
  ind <- .rs.readUiPref("num_spaces_for_tab")
  if (n_nested > 0 & getOption("turbokit-smartpipe")) {
    rstudioapi::insertText(paste0(
      " ", pipe, "\n", strrep(" ", ind)
    ),
     #location = needed here, else ranges error
    location = rstudioapi::document_position(
      y$start["row"],
      col + n_nested
    )
    )
    rstudioapi::setCursorPosition(
      rstudioapi::document_position(y$start["row"] + 1, col)
    )
  } else {
    indent_c <-
      regexec(pattern = "\\w", x$contents[row], perl = TRUE)[[1]][1] - 1
    if (indent_c < 0) {
      ind <- nchar(x$contents[row])
    }
    if (sum(y$end - y$start) > 0) {
      rstudioapi::insertText(paste0(pipe, "\n", strrep(" ", ind)))
    }
    else {
      if (grepl(pattern = "\\s$", x$contents[row], perl = TRUE)) {
        rstudioapi::insertText(paste0(pipe, "\n", strrep(" ", ind)))
      }
      else {
        rstudioapi::insertText(paste0(" ", pipe, "\n", strrep(" ", ind)))
      }
    }
  }
}
