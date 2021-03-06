% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{superpipe}
\alias{superpipe}
\title{Instigate dynamic complex pipe snippet processing and insertion.}
\usage{
superpipe()
}
\value{
\code{\%>\%.} inserted at the current cursor location and a custom
snippet object \code{s} in the next row.
}
\description{
Instigate dynamic complex pipe snippet processing and insertion.
}
\section{Insert superpipe}{


The superpipe is the entry point for translation of pipemath. It is designed
to be called via shortcut, and is primarily called for its side effects.

\code{superpipe()} is a wrapper function that initiates a series of turbokit
function calls to translate pipemath into a dynamic snippet, and helps the
user to insert the newly created snippet directly into the R script.

Its side effects, including code formatting and auto-insertion are tweakable
through package options. Some of these options are operating system-specific.
See \code{vignette("Turbokit")} for available options and adjusting superpipe
output to your preference.

\bold{Formatting}
There are some limitations to automated formatting.
1.
multi-line pipemath statements require the superpipe operator \code{\%>>\%} to
be manually typed, and an arbitrary symbol to be inserted after ward like so
\code{\%>>\%.}.
}

