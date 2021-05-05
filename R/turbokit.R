#' turbokit: Speedy and dynamic code input.
#'
#' The turbokit package provides several features to speed up inputting code
#' and minimize redundant formatting tasks. Its main purposes is to aid the user
#' in doing data science efficiently. To do this, turbokit introduces alternative
#' input methods for functions in different packages, and has powerful chaining
#' abilities.
#'
#' A core consideration of the package is that each user is unique and has their own
#' preference and use case. For this reason, most functions' side effects are
#' customizable, and several approaches are offered that result in the same output.
#'
#' \code{vignette("tidymath")}
#' \code{vignette("superpipe")}
#'
#' @section
#' Setting up turbokit:
#' To get the most out of turbokit, the user should bind its core functions to shortcuts.
#' This can be done by navigating from RStudio:
#' \strong{Tools -> Addins -> Browse Addins -> Keyboard Shortcuts}
#'
#' Suggestion for bindings, for minimal hand travel distance and natural combinations.
#' \tabular{lcr}{
#' \bold{Function} \tab \bold{Recommended Shortcut} \tab Description\cr
#' construct_complex \tab \emph{ctrl+shift+/} \tab  insert 1500+ functions\cr
#' insert_pipe \tab \emph{ctrl+shift+.} \tab  insert pipe based on context\cr
#' toggle_pipe \tab \emph{ctrl+shift+,} \tab  switch between pipes\cr
#' toggle_mode \tab \emph{ctrl+shift+right} \tab  switch between shortcut modes\cr
#' select \tab \emph{ctrl+shift+s} \tab  insert dplyr::select\cr
#' filter \tab \emph{ctrl+shift+f} \tab  insert dplyr::filter\cr
#' mutate \tab \emph{ctrl+shift+m} \tab  insert dplyr::mutate\cr
#' summarise \tab \emph{ctrl+shift+z} \tab  insert dplyr::summarise\cr
#' across \tab \emph{ctrl+shift+a} \tab  insert tiyselect::across\cr
#' contains \tab \emph{ctrl+shift+f} \tab  insert tiyselect::across\cr
#' starts_with \tab \emph{ctrl+shift+1} \tab insert tiyselect::across\cr
#' ends_with \tab \emph{ctrl+shift+2} \tab  insert tiyselect::across\cr
#' }
#'
#' \emph{This is a mere recommendation - users are highly encouraged to try out
#' different setupts that work for them.}
#'
#' @section
#' \code{insert} functions:
#' The \code{insert} functions insert a verb or operator in the R script at the current cursor
#' location, and place the cursor between the function's brackets. The insert_ functions are
#' designed to be called with shortcuts, allowing for rapid insertion of code.
#'
#' @section
#' \code{construct} functions:
#' The \code{construct} functions are a family of insert functions of a certain
#' package. Each function is formed by an abbreviation of snakecase initials.
#' Construct functions describe most functions of a packages, bar those with
#' identical initials, in which case the more popular one is implemented.
#'
#' The \code{construct_complex} function is a wrapper for all other construct
#' functions, and its shortcut bindings are dynamic. This means that the user
#' can specify preferences, changing the shortcuts required to call functions from
#' different packages. In other words, per user definition, the same shortcut
#' combinations may yield different outputs, and this change can be tightly controled
#' by the user.
#'
#' @section
#' toggle_ functions:
#' The toggle_ function provides a shortcut to change package options, which
#' changes the side effects of key turbokit functions.
#'
#' 1. \code{toggle_pipe}
#'
#' This function toggles between widely used pipe operators \%>\% and +. This is
#' often called under the hood by several turbokit functions to switch between
#' operators based on the document context. In effect, it greatly reduces the chance
#' to input the wrong operator. This function is exported.
#'
#' 2. \code{toggle_mode}
#'
#' This function toggles between \code{turbokit modes}. A turbokit mode defines
#' the output of the \code{construct_complex} function. Each mode comes with
#' several predefined packages and shortcut abbreviations.
#'
#' @docType package
#' @name turbokit
NULL
# > NULL


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
