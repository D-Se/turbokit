expand_tidyverse_default <- function(x) {
  if (nchar(x) == 3) {
    x <- stringi::stri_c(x, collapse = "")
    expression <- switch(substr(x, start = 1, stop = 3),
      "cda" = "cur_data_all",
      "cgr" = "cur_group_rows",
      "adf" = "as_data_frame",
      "igd" = "is_grouped_df",
      "ngd" = "new_grouped_df",
      "vgd" = "validate_grouped_df",
      {
        message("No default word found. Did you forget a leading digit?")
        NA
      }
    )
  } else if (nchar(x) == 2) {
    x <- stringi::stri_c(x, collapse = "")
    expression <- switch(substr(x, start = 1, stop = 2),
      "sw" = "starts_with",
      "ew" = "ends_with",
      "rj" = "right_join",
      "lj" = "left_join",
      "cw" = "case_when",
      "ao" = "any_of", # all_of
      "nr" = "num_range", # nrow
      "ac" = "add_count", # add_column (tibble)
      "ar" = "add_row",
      "at" = "add_tally", # as_tibble
      "aj" = "anti_join",
      "bc" = "bind_cols",
      "br" = "bind_rows",
      "ca" = "c_across",
      "cb" = "common_by",
      "cd" = "cur_data",
      "cc" = "cur_column",
      "cg" = "cur_group",
      "cm" = "colMeans",
      "cs" = "colSums",
      "df" = "data.frame",
      "fj" = "full_join",
      "fc" = "file.choose",
      "gc" = "group_cols",
      "gd" = "group_data",
      "gi" = "group_indices",
      "gk" = "group_keys",
      "gm" = "group_map", # experimental
      "gn" = "group_nest",
      "gs" = "group_size", # group_split
      "gt" = "group_trim",
      "gv" = "group_vars",
      "gw" = "group_walk",
      "ia" = "if_all",
      "ie" = "if_else",
      "in" = "is.numeric",
      "if" = "is.factor",
      "il" = "is.list",
      "ic" = "is.character",
      "ii" = "is.integer",
      "im" = "is.matrix",
      "ip" = "install.packages",
      "ij" = "inner_join",
      "lc" = "last_col",
      "nc" = "ncol",
      "nd" = "n_distinct",
      "ng" = "n_groups",
      "ni" = "na_if",
      "nb" = "nest_by",
      "nj" = "nest_join",
      "rw" = "rename_with",
      "rd" = "rows_delete",
      "ri" = "rows_insert",
      "rp" = "rows_patch",
      "ru" = "rows_update", # rows_upsert
      "rs" = "rowSums",
      "rc" = "read_csv",
      "rm" = "rowMeans",
      "ss" = "set.seed", # same_src slice_sample
      "sf" = "sample_frac",
      "sj" = "semi_join",
      "se" = "setequal", # weird naming convention
      "sd" = "setdiff",
      "sh" = "slice_head",
      "sm" = "slice_max", # slice_min
      "st" = "slice_tail",
      "tm" = "trunc_mat",
      "ts" = "type_sum",
      "wo" = "with_order",
      "wg" = "with_groups",
      "wl" = "writeLines",
      {
        message("No default word found. Did you forget a leading digit?")
        NA
      }
    )
  } else if (nchar(x == 1)) {
    expression <- switch(substr(x, start = 1, stop = 1),
      "m" = "mutate", # matches
      "f" = "filter",
      "s" = "select", # slice
      "p" = "ggplot", # pull
      "z" = "summarise",
      "a" = "across", # arrange
      "e" = "everything", # explain
      "g" = "group_by", # glimpse
      "c" = "contains", # coalesce (dplyr), collect, combine
      "r" = "rowwise", # relocate rename
      "b" = "between",
      "d" = "distinct",
      "i" = "intersect",
      "l" = "length", # lead , lag
      "n" = "near", # nth
      "t" = "transmute",
      "u" = "ungroup",
      "v" = "View", # base function, consider utils::view
      {
        message("No default word found. Did you forget a leading digit?")
        NA
      }
    )
  }
}
##########  ggplot2 ##########
#' Function to insert ggplot shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggplot <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^g|s|c|f|e|p|t{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter, currently recognized
                    ggplot starting chars:", ggplot_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    # user not in "+" mode and plot / geom / coord / facet / scaleon script line
    if (check_plot_context()) invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggplot_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggplot_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 4)
  switch(x[1],
    "g" = expand_ggplot_geom(x),
    "s" = expand_ggplot_scale(x),
    "c" = expand_ggplot_coord(x),
    "f" = expand_ggplot_facet(x),
    "e" = expand_ggplot_element(x),
    "p" = expand_ggplot_position(x),
    "t" = expand_ggplot_theme(x),
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown ggplot abbreviation"
      ))
      NA
    }
  )
}

expand_ggplot_scale <- function(x) {
  out <- character(length(x))
  out[1] <- "scale"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "x" = "x",
      "y" = "y",
      "c" = "colour",
      "f" = "fill",
      "l" = "linetype",
      "s" = "size",
      "a" = "alpha",
      "d" = "discrete",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown ggplot_scale abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  if (length(out) >= 3) {
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = expand_ggplot_scale_tertiary(x[3]),
      fixed = TRUE
    )
  }
  if (length(out) == 4) {
    stopifnot(x[4] %in% c("b", "c", "d"))
    out[4] <- x[4]
  }
  paste0(out, collapse = "_")
}

expand_ggplot_scale_tertiary <- function(y) {
  switch(y,
    "b" = "binned",
    "c" = "continuous",
    "d" = "discrete",
    "l" = "log10",
    "r" = "reverse",
    "s" = "sqrt", # steps
    "t" = "time",
    "m" = "manual",
    "g" = "gradient",
    "v" = "viridis",
    "g" = "grey",
    "h" = "hue",
    "f" = "fermenter",
    "i" = "identity",
    "o" = "ordinal",
    {
      message(paste(
        "third letter:",
        y,
        "unknown ggplot_scale abbreviation"
      ))
      NA
    }
  )
}

expand_ggplot_geom <- function(x) {
  stopifnot(length(x) >= 2 & length(x) < 4)
  out <- character(length(x))
  out[1] <- "geom"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "a" = "abline",
      "b" = "boxplot",
      "c" = "contour",
      "d" = "density_2d",
      "e" = "errorbar",
      "f" = "freqpoly",
      "h" = "histogram",
      "j" = "jitter",
      "l" = "linerange", # label
      "p" = "polygon",
      "q" = "quantile",
      "r" = "raster",
      "s" = "segment", # smooth
      "t" = "text",
      "v" = "violin",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown ggplot_geom abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  if (length(out) == 3) {
    stopifnot(x[3] == "f")
    out[3] <- "filled"
  }
  paste0(out, collapse = "_")
}

expand_ggplot_coord <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "coord"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    ### TODO: Order of appearance of coord calls
    replacement = switch(x[2],
      "f" = "flip", # fixed?
      "s" = "sf",
      "c" = "cartesian",
      "m" = "map", # munch?
      "p" = "polar",
      "q" = "quickmap",
      "e" = "equal",
      "t" = "trans",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown coord abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_ggplot_facet <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "facet"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "w" = "wrap",
      "g" = "grid",
      "n" = "null",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown facet abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_ggplot_element <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "element"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "t" = "text",
      "b" = "blank",
      "l" = "line",
      "g" = "grob",
      "r" = "rect", # render?
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown element abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_ggplot_position <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "position"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "d" = "dodge",
      "f" = "fill",
      "i" = "identity",
      "j" = "jitter", # jitterdodge?
      "n" = "nudge",
      "s" = "stack",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown position abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_ggplot_theme <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  if (length(out) == 1) {
    return("theme")
  }
  out[1] <- "theme"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "m" = "minimal",
      "l" = "light",
      "b" = "bw",
      "v" = "void",
      "c" = "classic",
      "d" = "dark",
      "g" = "gray",
      "r" = "replace",
      "s" = "set",
      "u" = "update",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown theme abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

##########  stringr ##########
#' Function to insert stringr shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
# #' @export
construct_stringr <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^s|w{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized stringr starting chars:", stringr_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle_pipe() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_stringr_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_stringr_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 3)
  switch(x[1],
    "s" = expand_stringr_str(x),
    "w" = "word",
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown stringr abbreviation"
      ))
      NA
    }
  )
}

expand_stringr_str <- function(x) {
  out <- character(length(x))
  out[1] <- "str"
  if (length(out) == 3) {
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "a" = "all",
        "d" = "data",
        "l" = "lower",
        "u" = "upper",
        "t" = "title",
        "f" = "fixed",
        "s" = "sentence",
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown stringr abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "r" = "remove", # replace
        "e" = "extract",
        "m" = "match",
        "l" = "locate",
        "g" = "glue",
        "t" = "to",
        "s" = "split",
        "v" = "view",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown stringr abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "l" = "length",
        "c" = "count",
        "d" = "detect",
        "e" = "extract",
        "f" = "flatten",
        "g" = "glue",
        "m" = "match",
        "o" = "order",
        "p" = "pad",
        "r" = "remove", # replace
        "s" = "split", # subset
        "t" = "trim",
        "v" = "view",
        "w" = "wrap",
        {
          message(paste(
            "second letter:",
            x[1],
            "unknown stringr abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

##########  clock ##########
#' Function to insert clock shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
## ' @export
construct_clock <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^a|d|g|i|s|w|z{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized clock starting chars:", clock_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle_pipe() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_clock_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_clock_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  switch(x[1],
    "a" = expand_clock_add(x), # as
    "d" = expand_clock_date(x),
    "g" = expand_clock_get(x),
    "i" = expand_clock_is(x),
    # "n" = "naive"
    "s" = expand_clock_set(x),
    "w" = expand_clock_weekday(x),
    "z" = expand_clock_zone(x),
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown clock abbreviation"
      ))
      NA
    }
  )
}

expand_clock_add <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "add"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "d" = "days",
      "h" = "hours",
      "m" = "minutes", # months, microseconds, miliseconds
      "n" = "nanoseconds",
      "q" = "quarters",
      "s" = "seconds",
      "w" = "weeks",
      "y" = "years",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown clock_add abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_clock_date <- function(x) {
  stopifnot(length(x) <= 4)
  out <- character(length(x))
  out[1] <- "date"
  if (length(out) == 3) {
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "y" = "year",
        "z" = "zone",
        "f" = "factor", # floor
        "p" = "parse",
        "b" = "build",
        {
          message(paste(
            "second letter:",
            x[1],
            "unknown clock_date abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "l" = "leap",
        "m" = "month",
        "s" = "set", # floor
        "w" = "weekday",
        "t" = "time",
        {
          message(paste(
            "third letter:",
            x[1],
            "unknown clock_date abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "b" = "build",
        "c" = "ceiling",
        "f" = "format", # floor
        "g" = "group",
        "p" = "parse",
        "r" = "round",
        "s" = "shift",
        "z" = "zone",
        {
          message(paste(
            "second letter:",
            x[1],
            "unknown clock_date abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_clock_get <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "get"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "y" = "year",
      "m" = "minute", # months, microseconds, miliseconds
      "d" = "day",
      "w" = "week",
      "i" = "index",
      "q" = "quarter",
      "s" = "second",
      "n" = "nanosecond",
      {
        message(paste(
          "second letter:",
          x[1],
          "unknown clock_get abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_clock_is <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "is"
  if (length(out) == 5) {
    out <- "is_iso_year_week_day"
  } else if (length(out) == 4) {
    out[2] <- "year"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "m" = "month",
        "q" = "quarter",
        "w" = "week",
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown clock_is abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    out[4] <- sub(
      x = x[4],
      pattern = x[4],
      replacement = switch(x[4],
        "d" = "day",
        "w" = "weekday",
        {
          message(paste(
            "fourth letter:",
            x[4],
            "unknown clock_is abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (length(out) == 3) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "y" = "year",
        "n" = "naive",
        "s" = "sys",
        "z" = "zoned",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown clock_is abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    out[3] <- ifelse(x[3] == "t", "time", "day")
  } else {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "d" = "duration",
        "w" = "weekday",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown clock_is abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_clock_set <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "set"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "y" = "year",
      "m" = "minute", # months, microseconds, miliseconds
      "d" = "day",
      "w" = "week",
      "i" = "index",
      "q" = "quarter",
      "s" = "second",
      "n" = "nanosecond",
      {
        message(paste(
          "second letter:",
          x[1],
          "unknown clock_set abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_clock_weekday <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "weekday"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "code",
        "f" = "factor",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown clock_set abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_clock_zone <- function(x) {
  stopifnot(length(x) <= 4)
  out <- character(length(x))
  out[1] <- "zoned"
  out[2] <- "time"
  if (length(out) == 4) {
    out[3] <- ifelse(x[3] == "p", "parse", "set")
    out[4] <- dplyr::case_when(
      x[4] == "a" ~ "abbrev",
      x[4] == "c" ~ "complete",
      x[4] == "z" ~ "zone"
    )
  } else if (length(out) == 3) {
    if (out[1] == "zone") {
      out[3] <- ifelse(x[3] == "n", "names", "version")
    } else {
      out[3] <- sub(
        x = x[3],
        pattern = x[3],
        replacement = switch(x[3],
          "n" = "now",
          "p" = "precision",
          "z" = "zone",
          {
            message(paste(
              "third letter:",
              x[1],
              "unknown clock_zone abbreviation"
            ))
            NA
          }
        ),
        fixed = TRUE
      )
    }
  }
  paste0(out, collapse = "_")
}

##########  forcats ##########
#' Function to insert forcats shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
# #' @export
construct_forcats <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^d|a|l{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized forcats starting chars:", forcats_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle_pipe() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))

  expression <- expand_forcats_abbreviation(abb)

  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_forcats_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  switch(x[1],
    "f" = expand_forcats_fct(x),
    "a" = "as_factor", # as
    "l" = expand_forcats_lvls(x),
    {
      message(paste("first letter:", x[1], "unknown forcats abbreviation"))
      NA
    }
  )
}

expand_forcats_fct <- function(x) {
  stopifnot(length(x) <= 3)
  out <- character(length(x))
  out[1] <- "fct"
  if (length(out) == 3) {
    if (x[2] == "e") {
      return("fct_explicit_na")
    }
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "p" = "prop",
        "l" = "lowfreq",
        "m" = "min",
        "n" = "n",
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown forcats_lump abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    out[2] <- "lump"
  } else if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "r" = "reorder", # rev, relabel, recode, relevel
        "l" = "lump",
        "d" = "drop", # months, microseconds, miliseconds
        "i" = "infreq", # inorder #inseq
        "e" = "expand",
        "a" = "anon",
        "c" = "collapse", # count, cross
        "m" = "match",
        "o" = "other",
        "s" = "shuffle", # shift
        "u" = "unique", # unify
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown forcats_fct abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_forcats_lvls <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "lvls" # if
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "r" = "reorder",
      "e" = "expand",
      "u" = "union",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown forcats_lvls abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

##########  readr ##########
#' Function to insert readr shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
# #' @export
construct_readr <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^r|w|t|c|d|f|m|p|s{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized readr starting chars:", readr_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  mode <- ifelse(pipe_toggle()$pipe == "%>%", 1, 0)
  if (!mode == 1) {
    warning("+ operator mode spotted while plotting.\nsetting mode to %>%, use turbokit::toggle_pipe() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(input, split = "", fixed = TRUE)))
  expression <- expand_readr_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_readr_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  switch(x[1],
    "r" = expand_readr_read(x),
    "w" = expand_readr_write(x),
    "t" = expand_readr_tokenize(x),
    "c" = expand_readr_col(x),
    "d" = expand_readr_date(x),
    "f" = expand_readr_format(x),
    "m" = expand_readr_melt(x),
    "p" = expand_readr_parse(x),
    "s" = expand_readr_spec(x),
    {
      message(paste("second letter:", x[2], "unknown readr abbreviation"))
      NA
    }
  )
}

expand_readr_read <- function(x) {
  stopifnot(length(x) <= 4)
  out <- character(length(x))
  out[1] <- "read"
  if (length(out) == 4) {
    out[2] <- "lines"
    out[3] <- "raw"
    out[4] <- "chunked"
  } else if (length(out) == 3) {
    out[3] <- ifelse(x[3] == "c", "chunked", "raw")
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv", # csv2
        "l" = "lines",
        "d" = "delim",
        "t" = "tsv",
        "f" = "file",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_read abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv",
        "t" = "table", # tsv
        "l" = "lines", # log
        "d" = "delim",
        "f" = "file", # fwf
        "r" = "rds",
        "b" = "builtin", # csv2
        "e" = "example",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_read abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_readr_write <- function(x) {
  stopifnot(length(x) <= 3)
  out <- character(length(x))
  out[1] <- "write"
  if (length(out) == 3) {
    out[3] <- "csv"
    out[2] <- "excel"
  } else if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv",
        "d" = "delim",
        "f" = "file",
        "l" = "lines",
        "r" = "rds",
        "t" = "tsv",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_write abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_readr_tokenize <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  if (length(out) == 1) {
    return("tokenize")
  }
  out[1] <- "tokenizer"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv",
        "d" = "delim",
        "f" = "fwf",
        "l" = "line",
        "t" = "tsv",
        "w" = "ws",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_tokenize abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_readr_col <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "col"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "c" = "character",
      "d" = "date", # datetime double
      "f" = "factor",
      "g" = "guess",
      "i" = "integer",
      "l" = "logical",
      "n" = "number",
      "s" = "skip",
      "t" = "time",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown readr_col abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_readr_date <- function(x) {
  stopifnot(length(x) <= 3)
  out <- character(length(x))
  out <- ifelse(length(out) == 3, "date_names_lang", "date_names")
}

expand_readr_format <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "format"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "c" = "csv",
      "d" = "delim",
      "t" = "tsv",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown readr_format abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_readr_melt <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "melt"
  if (length(out) == 3) {
    out[3] <- "chunked"
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv",
        "d" = "delim",
        "t" = "tsv",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_melt abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "csv",
        "d" = "delim",
        "t" = "tsv", # table
        "f" = "fwf",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown readr_melt abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_readr_parse <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "parse"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "c" = "character",
      "d" = "date", # date time double
      "f" = "factor",
      "g" = "guess",
      "i" = "integer",
      "l" = "logical",
      "n" = "number",
      "t" = "time",
      "v" = "vector",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown readr_parse abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_readr_spec <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "spec"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "c" = "csv",
      "d" = "delim",
      "t" = "tsv", # table
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown readr_spec abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}
