
#' Translates tidymath into snippet format.
#'
#' @param input a list of tidyverse verbs, each list element is a row in output
#' @return a new entry in r.snippets file called "s"
construct_snippet <- function(input) {
  ### TODO:vectorised stri_replace_all_fixed here stringi::stri_detect_fixed(out[i], "0")
  out <- stringr::str_replace_all(string = input, pattern = NAMED_PATTERNS)
  m <- 1
  # irretrievable object from here, syntax starts with $
  for (i in seq_along(out)) {
    num <- sum(stringi::stri_count(out[i], fixed = "0"))
    # if multiple snippet positions detected - split into subroutine
    if (num > 1) {
      temp <- unlist(stringi::stri_split(str = out[i], regex = "(?<=[=])"))
      build <- character(length = length(temp))
      for (j in seq_along(temp)) {
        inter <- temp[j]
        build[j] <- sub(
          x = inter,
          pattern = "0",
          replacement = as.character(m)
        )
        m <- m + 1
      }
      out[i] <- stringi::stri_c(build, collapse = "")
    } else {
      out[i] <- stringi::stri_replace_all_fixed(
        str = out[i],
        pattern = "0",
        replacement = as.character(m)
      )
      m <- m + 1
    }
  }
  add_snippet(
    name = "s",
    body = out
  )
}


# Obtain platform-specific snippets file path
get_snippets_path <- function() {
  if (!rstudioapi::isAvailable()) {
    # soft error for .onLoad and .onAttach to silently fail if called from R.
    return("error")
  } else if (identical(rstudioapi::versionInfo()$mode, "server")) {
    # cloud currently not supported
    return("error")
  }
  # snippet directory changes in RStudio 1.3
  VER <- rstudioapi::getVersion() >= "1.3.0"
  OS <- .Platform$OS.type
  #OS <- Sys.info()["sysname"]
  if (OS == "windows") {
    if (VER) {
      out <- paste0(Sys.getenv()["APPDATA"], "\\RStudio\\snippets\\r.snippets")
    }
  } else if (OS == "unix") {
    if (VER) {
      out <- file.path("~/.config/RStudio/snippets/r.snippets")
      if (!file.exists(out)) {
        warning("no snippet directory found")
        return("error")
      }
      #out <- rappdirs::user_config_dir("RStudio", os = "unix")
    } #else {
      #out <- fs::path_home_r(".R", "snippets.R")
    #}
  }
  out
}

### snippr functions (altered for performance and adapted to TK use case, not exported)
### Original author: David Robinson, under GPL 2 license. Emails to update package
### went unanswered.

# Function to add snippet to a R.snippets file
add_snippet <- function(name, body) {
  path <- getOption("turbokit-snippetdir")
  if (path == "error") {
    stop("snippet file path corrupt, function disabled.")
  }
  current <- read_snippet(path = path)
  current[[name]] <- prepare_snippet(body)
  write_snippet(current, path = path)
}

# Format character input to proper indentation
prepare_snippet <- function(body) {
  lines <- do.call(c, strsplit(
    x = body,
    split = "\\n",
    fixed = TRUE
  ))
  if (!all(stringi::stri_detect_regex(lines[lines != ""], "^\t"))) {
    lines <- paste0("\t", lines)
  }
  # end with a length 1 character vector
  paste(lines, collapse = "\n")
}

# Function to obtain the snippets present in a R.snippets file
read_snippet <- function(path) {
  lines <- readLines(path)
  lines <- do.call(c, stringi::stri_split(str = lines, regex = "\\n"))
  d <- data.frame(line = lines)
  d$lines <- stringi::stri_match(
    str = d$line,
    regex = "^snippet (.*)"
  )[, 2]
  d$snippet <- stringi::stri_match(
    str = d$line,
    regex = "^snippet (.*)"
  )[, 2]
  d$group <- cumsum(!is.na(d$snippet))
  q <- split(x = d, f = d$group)
  snippets <- lapply(q, function(x) paste(x$line[-1], collapse = "\n"))
  # remove missing snippets
  snippets <- Filter(function(x) x != "", snippets)
  names(snippets) <- d$snippet[!is.na(d$snippet)]
  snippets
}

# Function to write to a R.snippets file
write_snippet <- function(snippets, path) {
  snippet_txt <- paste0("snippet ", names(snippets), "\n",
    as.character(snippets),
    collapse = "\n"
  )
  writeLines(snippet_txt, path)
}

# Function to remove a given snippet from a R.snippets file.
remove_snippet <- function(name, path) {
  current <- read_snippet(path = path)
  current[[name]] <- NULL
  write_snippet(current, path = path)
}
