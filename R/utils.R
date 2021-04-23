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
    if (toggle_pipe()$pipe == "%>%"){
        context <- rstudioapi::getActiveDocumentContext()
        doc_text <- context$contents[context$selection[[1]]$range$end["row"]]
        if (grepl("plot\\(|aes\\(|geom\\_|coord\\_|facet\\_|scale_",
                  x = doc_text, perl = TRUE)) {
            if (warn) {
                warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
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
        if (!grepl("^[[:digit:]]{1}", abb, perl = TRUE)) {
            stop(paste("Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", .complex_error))
        } else if (grepl(" ", abb, perl = TRUE) | nchar(abb) > 6) {
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
        if (grepl("^[~]{1}", abb, perl = TRUE)) {
            message("complex input - computing verb inserts")
            # remove any non-accepted formatting
            out <- sub(pattern = "[^~>,*a-z0-9]", x = abb,
                       replacement = "",
                       perl = TRUE)
            if (abb != out) {
                message("Unrecognized formatting detected - coercing to accepted format")
            }
            # input is ready for chaining constructs and inserts
            return(out)
        } else if (!grepl("^[[:digit:]]{1}", abb, perl = TRUE)) {
            ### TODO: create direct call for user-defined favorite shortcuts
            # example: input ~p~ returns ggplot(aes()) + geom_boxplot()
            stop(paste("Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", .complex_error))
        } else {
            if (nchar(abb) > 6) {
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
