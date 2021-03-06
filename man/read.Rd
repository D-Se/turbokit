% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read}
\alias{read}
\title{Specify personal preference for shortcut functions}
\usage{
read(package, pos = NULL)
}
\arguments{
\item{package}{name of a package.}

\item{pos}{position to insert package at in list of options. If \code{NULL},
a guess to a position is made.}
}
\value{
Change in turbokit options, changing the behavior of \code{construct_complex}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function is is much like the well-known library function. It imports the
abbreviations of a package and overwrites the existing mode. See \code{?default()}
to undo its effects.
}
\examples{
# add a package to the search tree of construct_coplex function.
read(scales)

# note: one exception exists in the naming conventions used
read(ggplot)

# specify a specific location, to determine a prefix to the abbreviation.
read(stringr, 3)
}
