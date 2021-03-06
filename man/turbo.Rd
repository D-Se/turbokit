% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turbo.R
\name{turbo}
\alias{turbo}
\title{Wrapper constructor function to insert tidyverse functions at the cursor position}
\usage{
turbo(input = NULL)
}
\arguments{
\item{input}{optional character string to convert to script text at cursor location.}
}
\value{
A formatted function inserted at the cursor location
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{turbo()} is a wrapper for \code{construct_*} functions. It
allows for rapid insertion of many popular functions living in
different packages in a shorthand style, reducing time spent inputting and
formatting code.

It has powerful side effect, which is its formula-like \emph{pipemath}
translation mechanism,with which entire pipelines are constructed and converted
to \emph{dynamic snippets}.
See \code{vignette("construct")} for a list of combination functions.
See \code{\link{superpipe}} for guide on constructing dynamic tidyverse snippets.

\strong{Abbreviations}:

A leading digit indicates which package's function to insert. A digit is followed
by a \emph{non-formatted} series of first letters of snake_case segments,
in order of appearance.

\strong{Default values}:

Given default package options see \code{vignette("set_x_default")},
the leading digits refer to the following packages:

\tabular{lllll}{
1\tab - searches for \strong{ggplot} functions\cr
2\tab - searches for \strong{stringr} functions\cr
3\tab - searches for \strong{forcats} functions\cr
4\tab - searches for \strong{clock} functions\cr
5\tab - searches for \strong{readr} functions
}
}
\section{Construct family}{

The construct_* functions are intended to be called via
a user-defined shortcut. There are three reasons for this:
\enumerate{
\item It allows for quick chaining of inputs
\item Bypassing a library call at every session
\item Bypass the need to put quotes around the call
}

In a specific use case, the \code{construct_complex()} function is
synergistic with the superpipe operator
}

