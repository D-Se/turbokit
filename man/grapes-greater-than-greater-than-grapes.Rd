% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operators.R
\name{\%>>\%}
\alias{\%>>\%}
\title{Superpipe operator: translate pipemath to dynamic snippets.}
\usage{
x \%>>\% comment
}
\arguments{
\item{x}{a character string in pipemath format to convert}

\item{comment}{quasi-comment}
}
\value{
A dynamic snippet added to R.snippets file
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Executes translation of pipemath, construction of dynamic snippet and inserts
the snippet at the cursor location. Designed to called by shortcut.
}
\section{Superpipe use}{


\bold{The superpipe}

The operator translates a character string of \emph{pipemath} at the current cursor
location to a \emph{dynamic snippet}. It is available as an add-in shortcut,
which is where its utility and powers shine. The superpipe operator is not
designed to be typed by hand, as at its core, the \emph{turbokit} package
seeks to reduce button presses for frequently used functions and code formatting.
Without using the shortcut, the user must add \bold{something} of
length 1, after the operator.
The superpipe operator shortcut is available after package installation, but
unlike the regular pipe operator shortcut, is not able to be called executed
without loading the \emph{turbokit} package, for user safety reasons.

\bold{Dynamic snippets}

When called using its shortcut format, the operator translates a character
string of \emph{pipemath} at the current cursor location to a \emph{dynamic snippet}.
A feature of the snippet, a text macro, is that users can press TAB to move to
specified locations. Dynamic snippets differ from regular snippets in that
input locations may vary depending on 1) the user input in the script, and
2) superpipe-generated locations. The superpipe operator can be used to
avoid the quirks and alien syntax of the snippet system while still enjoying
its benefits. The superpipe includes a guessing mechanism that, if no locations
are specified by the user, will insert locations based on common use cases.

The dynamic snippets are synergistic with \code{turbbokit}'s \code{construct_complex}
function, which is the translation engine that converts \emph{pipemath}. This
means that when going through inputs, the user can call the \code{construct_complex}
shortcut to insert function calls, without breaking the snippet.

The superpipe operator cleans up after itself. While the snippets remain available
until the snippet page is refreshed by restarting the R session or manually
opening and saving the snippet page, the superpipe-created snippet \emph{s}
is removed from the R.snippets file.

\bold{quasi-commenting}

The seemingly arbitrary input to be inserted after the superpipe operator is
there to satisfy a constraint to user-defined operators, which may not be unary.
A side effect is that the input is ignored entirely and has no effect on the
workings of the superpipe. As such, this piece of input can serve as a
nicely specially formatted comment, here called quasi-comment, which is not
evaluated yet syntax highlighted as if it were normal code.

Without the shortcut, the user can manually type \bold{almost any} input
of length 1 after the operator. Bar keywords like \code{for} or \code{in} and
the exclamation mark, any input is viable if approached cleverly.
This behavior extends to markdown. Note - it could lead to unintended behavior
}

