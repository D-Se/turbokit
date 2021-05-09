##########  insert_ helpers ##########

# Function to insert any function into an R script at the cursor position
insert <- function(verb) {
    verb <- deparse(substitute(verb))
    if (verb %in% c("starts_with", "contains", "ends_with")) {
        return(
            rstudioapi::insertText(paste0(verb,"(\"\")"))
        )
    }
    rstudioapi::insertText(paste0(verb, "()"))
}

# Function to insert function into an R script at the cursor position
insertmulti <- function(verb1, verb2) {
    verb1 <- deparse(substitute(verb1))
    verb2 <- deparse(substitute(verb2))
    rstudioapi::insertText(paste0(verb1, "(", verb2, "())"))
}

# Function to move mouse cursor based on current position
reposition <- function(n) {
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::setCursorPosition(
        rstudioapi::document_position(x$selection[[1]]$range$start["row"],
                                      x$selection[[1]]$range$end["column"] - n))
}

# Function to move mouse cursor to the end of the line
reposition_end <- function() {
    x <- rstudioapi::getActiveDocumentContext()
    y <- x$selection[[1]]$range
    row <- y$end["row"]
    col <- y$end["column"]
    n_nested <- sum(gregexpr(
        pattern = "[)\"]",
        text = x$contents[row],
        perl = TRUE
    )[[1]] > 0)
    if (n_nested > 0) {
            rstudioapi::insertText(" ", location = rstudioapi::document_position(
                y$start["row"],
                col + n_nested
            ))
        return(
            rstudioapi::setCursorPosition(
                rstudioapi::document_position(
                    y$start["row"],
                    col + n_nested + 1
                )
            )
        )
    } else {
        rstudioapi::setCursorPosition(
            rstudioapi::document_position(x$selection[[1]]$range$start["row"],
                                          x$selection[[1]]$range$end["column"] + 1))
    }
}

# Function to move mouse cursor to the next row
reposition_row <- function() {
    x <- rstudioapi::getActiveDocumentContext()
    rstudioapi::setCursorPosition(
        rstudioapi::document_position(x$selection[[1]]$range$start["row"] + 1,
                                      x$selection[[1]]$range$end["column"]))
}

# Input string at position
reposition_str <- function(string, inject, index) {
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

##########  construct_ helpers ##########

# Function to detect wrong pipe settings when constructing plot
check_plot_context <- function(warn = T) {
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

# Function that determines input to construct_ functions
complex_dialog <- function() {
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    tryCatch(expr = {
        if (rlang::is_empty(abb)) {
            stop(NULL)
        }
        if (!grepl(x = abb, pattern = "^[[:alnum:]]{1}", perl = TRUE)) {
            stop(paste("BBB Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", complex_error))
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

# Transformation step of dialog box input to be construct_* function-friendly
get_complex_userinput <- function() {
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
            return("clean")
        } else if (grepl("^!{1}", abb, perl = TRUE)) {
            return("read")
        } else if (grepl("^@{1}", abb, perl = TRUE)) {
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
        } else if (!grepl(pattern = "^[[:alnum:]]{1}", x = abb, perl = TRUE)) {
            ### TODO: create direct call for user-defined favorite shortcuts
            # example: input ~p~ returns ggplot(aes()) + geom_boxplot()
            stop(paste("AAA Invalid input: Non-numeric at start of wrapper function.
                    In complex wrapper, please use one of", complex_error))
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

# Transformation step of dialog box input to be construct_* function-friendly
transform_complex_interaction <- function(x) {
    if (sum(x == "*") == 1) {
        words <- x[!x == "*" & !x == ">"]
        outer <- x[grep("[*]", x, perl = TRUE) - 1]
        inner <- x[grep("[*]", x, perl = TRUE) + 1]
        n <- gregexpr("[(]", outer, perl = TRUE)[[1]][1]
        out <- reposition_str(outer, inner, n+1)
    } else {
        words <- x[!x == "*" & !x == ">"]
        inner <- words[3]
        outer <- words[2]
        n <- gregexpr("[(]", outer, perl = TRUE)[[1]][1]
        m <- gregexpr("[(]", words[1], perl = TRUE)[[1]][1]
        # place one term within brackets of the other
        out <- reposition_str(words[1],
                               reposition_str(outer, inner, n + 1), m + 1)
    }
    out <- c(out, ">")
    out
}

# Transformation step of dialog box input to be construct_* function-friendly
transform_complex_input <- function(x) {
    x <- sub(x = x, pattern = ".", replacement = "", perl = TRUE)
    unlist(strsplit(x = x, split = "", fixed = TRUE))
}
