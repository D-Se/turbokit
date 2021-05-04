##########  insert_ helpers ##########

#' Function to insert any function into an R script at the cursor position
#'
#' @param verb The verb to insert, as a name.
#' @return Verb as function call in R script of the user.
.insert <- function(verb) {
    verb <- deparse(substitute(verb))
    rstudioapi::insertText(paste0(verb, "()"))
}

#' Function to insert function into an R script at the cursor position
#'
#' @param verb1 The verb to insert, supplied in name form.
#' @param verb2 A second verb, to be placed within parenthesis of the first verb,
#' in name form.
#'
#' @return Verb as function call in R script of the user.
.insertmulti <- function(verb1, verb2) {
    verb1 <- deparse(substitute(verb1))
    verb2 <- deparse(substitute(verb2))
    rstudioapi::insertText(paste0(verb1, "(", verb2, "())"))
}

#' Function to move mouse cursor based on current position
#'
#' @param n A number by which to horizontally move the mouse cursor, counting from right to left.
#' @return Adjusted cursor position in R script.
.reposition <- function(n) {
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::setCursorPosition(
        rstudioapi::document_position(x$selection[[1]]$range$start["row"],
                                      x$selection[[1]]$range$end["column"] - n))
}

#' Function to move mouse cursor to the end of the line
#'
#' @return Adjusted cursor position in R script.
.reposition_end <- function() {
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::setCursorPosition(
        rstudioapi::document_position(x$selection[[1]]$range$start["row"],
                                      x$selection[[1]]$range$end["column"] + 1))
}

#' Function to move mouse cursor to the next row
#'
#' @return Adjusted cursor position in R script.
.reposition_row <- function() {
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::setCursorPosition(
        rstudioapi::document_position(x$selection[[1]]$range$start["row"] + 1,
                                      x$selection[[1]]$range$end["column"]))
}

#' Input string at position
#'
#' @param string character string to inject another string in
#' @param inject character string to inject into the other string
#' @param index position in string where to inject the new string.
#' @return character vector stripped of any numeric indicator.
.reposition_str <- function(string, inject, index) {
    inject <- inject[order(index)]
    index <- sort(index)
    # expand string
    split <- substr(rep(string, length(index) + 1),
                    start = c(1, index),
                    stop = c(index - 1, nchar(string)))
    ord1 <- 2 * (1:length(split)) - 1
    ord2 <- 2 * (1:length(inject))
    paste(c(split, inject)[order(c(ord1, ord2))], collapse = "")
}

##########  snippet helpers ##########

#' Obtain platform-specific snippets file path
#'
#' @return file path
.get_snippets_path <- function(){
    ### TODO: insert R studio internal file directory C code here?
    if (Sys.info()["sysname"] == "Windows") {
        paste0(Sys.getenv()["APPDATA"], "\\RStudio\\snippets\\r.snippets")
    } else {
        file.path("~", ".R", "snippets", "/r.snippets")
    }
}

### snippr functions (altered for performance and adapted to TK use case, not exported)
### Original author: David Robinson, under GPL 2 license

#' Function to add snippet to a R.snippets file
#'
#' @param name Name of the snippet to be added
#' @param body Body of the snippet to be added
#' @return A snippet inserted in the .snippet file
.add_snippet <- function(name, body) {
    path <- getOption("turbokit-snippetdir")
    current <- .read_snippet(path = path)
    current[[name]] <- .prepare_snippet(body)
    .write_snippet(current, path = path)
}

#' Format character input to proper indentation
#'
#' @param body Body of the snippet to be added
#' @return Formatted character of length 1 containing the snippet body
.prepare_snippet <- function(body) {
    lines <- do.call(c, strsplit(x = body,
                                 split = "\\n",
                                 fixed = TRUE))
    if (!all(stringi::stri_detect_regex(lines[lines != ""], "^\t"))) {
        lines <- paste0("\t", lines)
    }
    # end with a length 1 character vector
    paste(lines, collapse = "\n")
}

#' Function to obtain the snippets present in a R.snippets file
#'
#' @param path filepath to the R.snippets file
#' @return A list of character vectors of length 1 containing snippet bodies and format
.read_snippet <- function(path) {
    lines <- readLines(path)
    lines <- do.call(c, stringi::stri_split(str = lines, regex = "\\n"))
    d <- data.frame(line = lines)
    d$lines <- stringi::stri_match(
        str = d$line,
        regex = "^snippet (.*)")[, 2]
    d$snippet = stringi::stri_match(
                        str = d$line,
                        regex = "^snippet (.*)")[, 2]
    d$group = cumsum(!is.na(d$snippet))
    q <- split(x = d, f = d$group)
    snippets <- lapply(q, function(x) paste(x$line[-1], collapse = "\n"))
    # remove missing snippets
    snippets <- Filter(function(x) x != "", snippets)
    names(snippets) <- d$snippet[!is.na(d$snippet)]
    snippets
}

#' Function to write to a R.snippets file
#'
#' @param snippets list of character length 1 of snippets to be written to a file.
#' @param path file path to the snippet directory
#' @return Renewed R.snippet file.
.write_snippet <- function(snippets, path) {
    snippet_txt <- paste0("snippet ", names(snippets), "\n",
                          as.character(snippets), collapse = "\n")
    writeLines(snippet_txt, path)
}

#' Function to remove a given snippet from a R.snippets file.
#'
#' @param name Name of the snippet to be removed
#' @param path filepath of the snippet directory
#' @return A renewed R.snippets file without the snippet name specified in the call
.remove_snippet <- function(name, path) {
    current <- .read_snippet(path = path)
    current[[name]] <- NULL
    .write_snippet(current, path = path)
}

##########  construct_ helpers ##########

#' Function to detect wrong pipe settings when constructing plot
#'
#' @param warn Boolean if warning should be given when changing operators
#' @return Boolean TRUE if plot, geom, coord or facet is on line of R script where cursor is
.check_plot_context <- function(warn = T) {
    ### TODO getActiveDoc call or extract elements twice
    if (pipe_toggle()$pipe == "%>%"){
        context <- rstudioapi::getActiveDocumentContext()
        doc_text <- context$contents[context$selection[[1]]$range$end["row"]]
        if (grepl("plot\\(|aes\\(|geom\\_|coord\\_|facet\\_|scale_",
                  x = doc_text, perl = TRUE)) {
            if (warn) {
                warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle_pipe() or shortcut to switch back")
            }
            out <- TRUE
        } else {
            out <- FALSE
        }
    } else {
        out <- FALSE
    }
    out
}

#' Function that determines input to construct_ functions
#'
#' @return character vector of user inputs
.complex_dialog <- function() {
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    tryCatch(expr = {
        if (rlang::is_empty(abb)) {
            stop(NULL)
        }
        if (!grepl(x = abb, pattern = "^[[:alnum:]]{1}", perl = TRUE)) {
            stop(paste("BBB Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", .complex_error))
        } else if (grepl(" ", abb, perl = TRUE) | nchar(abb) > 10) {
            stop("Invalid input: space detected or input too long")
        }
        abb
    }, error = function(cond){
        message(cond)
        return("error")
    }
    )
}

#' Transformation step of dialog box input to be construct_* function-friendly
#'
#' @return character vector stripped of any numeric indicator
.get_complex_userinput <- function() {
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    tryCatch(expr = {
        if (rlang::is_empty(abb)) {
            stop(NULL)
        }
        if (grepl(" ", abb, perl = TRUE)) {
            stop("Invalid input: space detected - refrain from formatting abbreviations")
        }
        if (grepl("^>{1}", abb, perl = TRUE)) {
            return("boot")
        } else if (grepl("^<{1}", abb, perl = TRUE)) {
            return("default")
        } else if (grepl("^[~]{1}", abb, perl = TRUE)) {
            message("complex input - computing verb inserts")
            # remove any non-accepted formatting
            out <- sub(pattern = "[^~,*a-z0-9]", x = abb,
                       replacement = "",
                       perl = TRUE)
            if (abb != out) {
                message("Unrecognized formatting detected - coercing to accepted format")
            }
            # input is ready for chaining constructs and inserts
            return(out)
        } else if (!grepl(pattern = "^[[:alnum:]]{1}",x =  abb, perl = TRUE)) {
            ### TODO: create direct call for user-defined favorite shortcuts
            # example: input ~p~ returns ggplot(aes()) + geom_boxplot()
            stop(paste("AAA Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", .complex_error))
        } else {
            if (nchar(abb) > 10) {
                stop("Invalid input: input too long, no such abbreviations in dictionary")
            }
            return(abb)
        }
    }, error = function(cond){
        message(cond)
        return("error")
    }, warning = function(cond){
        message(cond)
        return("warning")
    }
    )
}

#' Transformation step of dialog box input to be construct_* function-friendly
#'
#' @param x character string starting with ~
#' @return character vector stripped of any numeric indicator
.transform_complex_interaction <- function(x) {
    if (sum(x == "*") == 1) {
        words <- x[!x == "*" & !x == ">"]
        outer <- x[grep("[*]", x, perl = TRUE) - 1]
        inner <- x[grep("[*]", x, perl = TRUE) + 1]
        n <- gregexpr("[(]", outer, perl = TRUE)[[1]][1]
        out <- .reposition_str(outer, inner, n+1)
    } else {
        words <- x[!x == "*" & !x == ">"]
        inner <- words[3]
        outer <- words[2]
        n <- gregexpr("[(]", outer, perl = TRUE)[[1]][1]
        m <- gregexpr("[(]", words[1], perl = TRUE)[[1]][1]
        # place one term within brackets of the other
        out <- .reposition_str(words[1],
                               .reposition_str(outer, inner, n+1), m+1)
    }
    out <- c(out, ">")
    out
}

#' Transformation step of dialog box input to be construct_* function-friendly
#'
#' @param x character string
#' @return character vector stripped of any numeric indicator
.transform_complex_input <- function(x) {
    x <- sub(x = x, pattern = ".", replacement = "", perl = TRUE)
    unlist(strsplit(x = x, split = "", fixed = TRUE))
}

#' Specify personal preference for shortcut functions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' This function is is much like the well-known library function. It imports the
#' abbreviations of a package and overwrites the existing setup. See \code{?default()}
#' to undo its effects.
#'
#' @param package name of a package.
#' @param pos position to insert package at in list of options. If \code{NULL},
#' a guess to a position is made.
#' @export
#' @return Change in turbokit options, changing the behavior of \code{construct_complex}
#' @examples
#' # add a package to the search tree of construct_coplex function.
#' read(scales)
#'
#' # note: one exception exists in the naming conventions used
#' read(ggplot)
#'
#' # specify a specific location, to determine a prefix to the abbreviation.
#' read(stringr, 3)
read <- function(package, pos = NULL) {
    package <- deparse(substitute(package))
    l <- vector(mode = "list", length = 9L)
    if (!package %in% .turbokit_packages) {stop("Package not recognized or not (yet) implemented")}
    l <- getOption("turbokit-up")
    if (is.null(pos)) {
        pos <- as.numeric(min(which(unlist(lapply(l, rlang::is_empty)) == T)))
    }
    if (!pos < 10 & pos != 0){stop("Position out of range, accepted positions: 1-9")}
    if (!is.numeric(pos)) {
        warning("position is not numeric, converting to numeric")
        pos <- as.numeric(pos)
    }
    l[[pos]] <- .create_expression(package = {{ package }})
    options("turbokit-up" = l)
    if (getOption("turbokit-verbose")) {
        cat(paste0(package, " inserted at position ", pos))
    }

}

#' Helper to restore default package options used for construct_complex package
#' abbreviations layout.
#'
#' @description
#' This function undos the action of the read function.
#'
#' @export
#' @return library call in script
#' @examples
#' # resetting the default
#' default()
default <- function() {
    options("turbokit-up" = {
        l <- vector(mode = "list", length = 9)
        names(l) <- letters[1:9]
        l
    })
}

#' Helper to transform character string into turbokit-recognized expression of package expansion
#'
#' @param package package name to transform into expression
#' @return library call in script
.create_expression <- function(package) {
    package <- rlang::ensym(package)
    term <- rlang::parse_expr(
        paste0(".expand_",
               rlang::expr_text(package),
               "_abbreviation(x)"
        )
    )
}

#' Helper for inserting library call if package is not yet loaded
#'
#' @description
#' This function is rarely directly called by the user. More often, it is called
#' through the \code{>} syntax of the \code{construct_complex} function and
#' automatically by the \code{%>>%} superpipe.
#'
#' @return library call in script
#' @export
#' @examples
#' # when manually inserting library calls
#' toggle_mode(mode_op = "tidyverse")
#' boot()
#'
#' # alternatively, the function can be called by using \code{>} or \code{boot}
#' shortcuts in the \code{construct_complex} function.
#' construct_complex("boot")
boot <- function(){
    up <- getOption("turbokit-up")
    default_up <- {
        l <- vector(mode = "list", length = 9)
        names(l) <- letters[1:9]
        l
    }
    if (!identical(up, default_up, ignore.environment = T, ignore.bytecode = T, ignore.srcref = T)) {
        packages <- as.character(unlist(up))
        if (!"turbokit" %in% .packages()) {
            library(turbokit)
        }
        packages <- paste0("library(", gsub(".*_(.+)_.*", "\\1", packages), ")")
        invisible(rstudioapi::insertText(packages))
    } else {
        mode <- mode_toggle()$mode
        switch(mode,
               "tidyverse" = invisible(
                   rstudioapi::insertText("library(tidyverse)")),
               "tidymodels" = invisible(
                   rstudioapi::insertText("library(tidymodels)")),
               "shiny" = invisible(rstudioapi::insertText("library(shiny)")),
               "dev" = invisible(rstudioapi::insertText("library(dev)"))
        )
    }
}

#' Clean the script of turbokit syntax
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Turbokit comes with several features that require special syntax.
#' Remnants of such syntax are not useful to the data science process. The clean
#' function is an exit function that transforms any sentences with tidymath
#' format into comments so that it will not be evaluated when running the script.
#' This is especially helpful for markdown files.
#'
#' Additionally, it detaches the turbokit package from the namespace.
#'
#' @param detach Boolean - detach the turbokit package after cleaning?
#' @export
#' @return Commented turbokit syntax
#' @example
#' # when there is turbokit syntax present in the active document
#' clean()
clean <- function(detach = TRUE){
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::sendToConsole(.cleaner(doc = x, detach = detach), execute = T, focus = F)
}

#' Clean the script of turbokit syntax
#'
#' @return library call in script
.cleaner <- function(doc, detach) {
    clean_ind <- which(stringi::stri_detect(str = doc$contents, regex = "%>>%") == T)
    for (i in seq_along(clean_ind)) {
        rstudioapi::modifyRange(id = doc$id,
                                text = "# ",
                                location = rstudioapi::document_position(
                                    row = clean_ind[i], column = 1))
    }
    if (detach) {
        devtools::unload("turbokit")
    }
}
