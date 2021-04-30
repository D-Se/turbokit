#' turbokit: Speedy tidyverse and pipe input
#'
#' The turbokit package provides two categories of important functions:
#' insert_ and toggle_.
#'
#' @section
#' Setting up turbokit:
#' Setting up turbokit requires the user to bind shortcuts to function calls.
#' This can be done by navigating from RStudio:
#' \strong{Tools -> Addins -> Browse Addins -> Keyboard Shortcuts}
#'
#' Suggestion for bindings, for minimal hand travel distance and natural combinations.
#' \tabular{lc}{
#' Function \tab Recommended Shortcut\cr
#' toggle \tab \emph{ctrl+shift+/}\cr
#' pipe \tab \emph{ctrl+shift+.}\cr
#' select \tab \emph{ctrl+shift+s}\cr
#' filter \tab \emph{ctrl+shift+f}\cr
#' mutate \tab \emph{ctrl+shift+m}\cr
#' summarise \tab \emph{ctrl+shift+z}\cr
#' across \tab \emph{ctrl+shift+a}\cr
#' contains \tab \emph{ctrl+shift+f}\cr
#' starts_with \tab \emph{ctrl+shift+1}\cr
#' ends_with \tab \emph{ctrl+shift+2}\cr
#' }
#'
#' @section
#' insert_ functions:
#' The insert_ functions insert a verb or operator in the R script at the current cursor
#' location, and place the cursor between the function's brackets. The insert_ functions are
#' designed to be called with shortcuts, allowing for successive shortcut calls.
#'
#' @section
#' toggle_ functions:
#' The toggle_ function provides a shortcut to change package options, which
#' makes conditional verb insertion possible. This is used to switch between
#' widely used pipe operators \%>\% and + with a button press. This button press
#' alters the output of another shortcut. This results in a general operator
#' shortcut button.
#'
#' @docType package
#' @name turbokit
NULL
#> NULL


# Adding new packages manual
# 1. Define numeric indicator (check if not already taken)
# 2. Make a construct_packagename skeleton
#   2.1 Find keywords (usually first letter of snake_cases)
#   2.2 Make error checks if key initials are not found (.packagename_error)
#       Entry in globals.R file ### error message strings
# 3. Make a generic .expand_packagename function
#       3.1 Find keywords (usually first letter of snake_cases)
#       3.2 Make .expand_packagename_keyword functions
#           3.2.1 Check for naming patterns (are there multiple with same initials?)
#           3.2.2 Ensure a NA exit if word not found
#           3.2.3 Put words in proper order (most popular first)
#       3.3 Make .expand_packagename_abbreviation wrapper
# 4. Finish construct_packagename function
# 5. Update globals, add patterns where needed
# 6. Update conustruct_complex function
# 7. If any new arithmatic in tidymath - update
#   7.1 update .get_complex_userinput (utils L 189)
#   7.2 if abbreviation legnth > 6, update .get_complex_userinput L206
#   7.3 Update tidymath
#       7.3.1 if new interaction term, update .transform complex interaction (utils)
#       7.3.2 if new function family, update .construct_complex L66
#       7.3.3 if multiple snippet position and not =, update .construct_snippet L147
# 8. Add construct_pckname to .construct_chain (construct)
# 9. Update documentation


# TODO make function factories for construct, expand abbreviation to easily
# expand package for feature request

# 1. describe family of packages (wrapper package) or package names
# 2. execute factories
# 3.

