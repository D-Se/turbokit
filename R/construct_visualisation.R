
#' Placeholder for default
#'
#' @param x character string of to compare to default abbreviations
#' @return Adjusted cursor position in R script
expand_visualisation_default <- function(x) {
  NULL
}

##########  ggmisc ##########
# several smaller plotting packages combined in one.
#

#' Function to translate several smaller addon packages shortcut combination
#' into R script at cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggmisc <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^a|c|g|p|s|e|t|u|f|r{1}", abb, perl = TRUE)) {
    message(paste(
      "Invalid input: unrecognized starting letter,
                    recognized misc ggplot addon packages starting chars:",
      ggmisc_error
    ))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggmisc_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggmisc_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 6)
  # collection of small visualization packages
  # ggfortify, ggsignif
  out <- character(length(x))
  switch(x[1],
    "a" = "annotate_textp", # ggalt
    "c" = "coord_proj", # ggalt
    "g" = expand_ggmisc_geom(x), # ggrepel
    "p" = expand_ggmisc_position(x),
    "s" = expand_ggmisc_scale(x),
    # "s" = expand_ggmisc_stat(x),
    "e" = expand_ggmisc_element(x),
    "t" = expand_ggmisc_theme(x),
    "u" = "unscale", # ggfortify
    "f" = {
      dplyr::case_when(
        x[2] == "m" ~ "fortify_map", # ggfortify
        x[2] == "p" ~ "ft_pal" # hrbrthemes
      )
    },
    "r" = {
      x <- stringi::stri_c(x, collapse = "")
      dplyr::case_when(
        x == "rg" ~ "removeGrid", # ggExtra
        x == "rgx" ~ "removeGridX", # ggExtra
        x == "rgy" ~ "removeGridY", # ggExtra
        x == "rtx" ~ "rotateTextX", # ggExtra
        x == "re" ~ "runExample" # ggExtra
      )
    },
    {
      message("unknown ggmisc abbreviation")
      NA
    }
  )
}

### TODO: ggfortify plots, survminer plots ggstatsplot
expand_ggmisc_geom <- function(x) {
  stopifnot(length(x) <= 6)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "glr" = "geom_label_repel", # ggrepel
    "gtr" = "geom_text_repel", # ggrepel
    "gbt" = "geom_bar_text", # ggfittext,
    "gft" = "geom_fit_text", # ggfittext
    "gs" = "geom_signif", # ggsignif
    "gr" = "geom_richtext", # ggtext
    "gt" = "geom_text", # ggtext
    "gb" = "geom_beeswarm", # ggbeeswarm
    "gc" = "geom_crossbarh", # ggstance
    "ge" = "geom_encircle", # ggalt
    "gh" = "geom_histogramh", # ggstance
    "gl" = "geom_lollipop", # ggalt
    "gv" = "geom_violinh", # ggalt
    "gq" = "geom_quasirandom", # ggbeeswarm,
    "gd" = "geom_dumbbell", # ggalt
    "gx" = "geom_xspline", # ggalt
    "gtw" = "geom_text_wordcloud", # ggwordcloud
    "gtwa" = "geom_text_wordcloud_area", # ggwordcloud,
    "ga" = "geom_alluvium", # ggalluvial
    "gf" = "geom_flow", # ggalluvial
    "gm" = "geom_mosaic", # ggmosaic
    "gmj" = "geom_mosaic_jitter", # ggmosaic
    "gmt" = "geom_mosaic_text", # ggmosaic
    "gbs" = "ggbetweenstats", # ggstatsplot
    "gbp" = "ggbiplot", # ggfortify
    "gcp" = "ggcpgram", # ggfortify
    "gfp" = "ggfreqplot", # ggfortify
    "gtd" = "ggtsdiag", # ggfortify
    "gac" = "ggadjustedcurves", # survminer
    "gcr" = "ggcompetingrisks", # survminer
    "gcd" = "ggcoxdiagnostics", # survminer
    "gcf" = "ggcoxfunctional", # survminer
    "gcz" = "ggcoxzph", # survminer
    "gcc" = "ggcumcensor", # survminer
    "gce" = "ggcumevents", # survminer
    "gfs" = "ggflexsurvplot", # survminer
    "grt" = "ggrisktable", # survminer
    "gse" = "ggsurvevents", # survminer
    "gsp" = "ggsurvplot", # survminer
    "gspaa" = "ggsurvplot_add_all", # survminer
    "gspf" = "ggsurvplot_facet", # survminer
    "gcs" = "ggcoefstats", # ggstatsplot
    "gcm" = "ggcorrmat", # ggstatsplot
    "ghs" = "gghistostats", # ggstatsplot
    "gps" = "ggpiestats", # ggstatsplot
    "gss" = "ggscatterstats", # ggstatsplot
    "gds" = "ggdotplotstats", # ggstatsplot
    "gws" = "ggwithinstats", # ggstatsplot
    {
      message("unknown ggmisc_geom abbreviation")
      NA
    }
  )
}

expand_ggmisc_element <- function(x) {
  stopifnot(length(x) <= 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "em" = "element_markdown", # ggtext
    "et" = "element_text", # ggtext
    "ets" = "element_textbox_simple", # ggtext
    {
      message("unknown ggmisc_element abbreviation")
      NA
    }
  )
}

expand_ggmisc_theme <- function(x) {
  stopifnot(length(x) <= 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "ti" = "theme_ipsum", # hrbrthemes
    "tfr" = "theme_ft_rc", # hrbrthemes
    "tie" = "theme_ipsum_es", # hrbrthemes
    "tip" = "theme_ipsum_ps", # hrbrthemes
    "tir" = "theme_ipsum_rc", # hrbrthemes
    "tit" = "theme_ipsum_tw", # hrbrthemes
    "tmr" = "theme_modern_rc", # hrbrthemes
    "tt" = "theme_tinyhand", # ggstatsplot
    "tc" = "theme_cormat", # ggstatsplot
    "tg" = "theme_ggstatsplot", # ggstatsplot
    "tp" = "theme_pie", # ggstatsplot
    "tct" = "theme_cleantable", # survminer
    "ts" = "theme_survminer", # survminer
    {
      message("unknown ggmisc_theme abbreviation")
      NA
    }
  )
}

expand_ggmisc_scale <- function(x) {
  stopifnot(length(x) <= 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "scf" = "scale_colour_ft", # hrbrthemes
    "sci" = "scale_colour_ipsum", # hrbrthemes
    "sff" = "scale_fill_ft", # hrbrthemes
    "sfi" = "scale_fill_ipsum", # hrbrthemes
    "sxc" = "scale_x_comma", # hrbrthemes
    "syc" = "scale_y_comma", # hrbrthemes
    "sxp" = "scale_x_percent", # hrbrthemes
    "syp" = "scale_y_percent", # hrbrthemes
    {
      message("unknown ggmisc_scale abbreviation")
      NA
    }
  )
}

expand_ggmisc_position <- function(x) {
  stopifnot(length(x) <= 5)
  x <- stringi::stri_c(x, collapse = "")

  switch(x,
    "pnr" = "position_nudge_repel", # ggrepel
    "pd" = "position_dodgev", # ggstance
    "pf" = "position_fillv", # ggstance
    "pj" = "position_jitterdodgev", # ggstance
    "ps" = "position_stackv", # ggstance,
    "pb" = "position_beeswarm", # ggbeeswarm,
    "pqr" = "position_quasirandom", # ggbeeswarm,
    "pq" = "position_quasirandom", # ggbeeswarm,
    {
      message("unknown ggmisc_position abbreviation")
      NA
    }
  )
}

##########  cowplot ##########

#' Function to translate cowplot package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_cowplot <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^g|p|d|s|t{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized cowplot starting chars:", cowplot_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_cowplot_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_cowplot_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 4)
  out <- character(length(x))
  if (grepl(x = x[1], pattern = "^g|p|d|s|t")) {
    switch(x[1],
      "g" = expand_cowplot_get(x),
      "p" = expand_cowplot_p(x),
      "d" = expand_cowplot_draw(x),
      "s" = expand_cowplot_stamp(x),
      "t" = expand_cowplot_theme(x),
      {
        message("unknown cowplot abbreviation")
        NA
      }
    )
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "as" = "add_sub",
      "and" = "agg_null_device",
      "am" = "align_margin",
      "ap" = "align_plots", # as_grob? as_gtable
      "ac" = "axis_canvas",
      "bg" = "background_grid",
      "cnd" = "cairo_null_device",
      "ckg" = "circle_key_glyph",
      "dfl" = "draw_figure_label",
      {
        message("unknown cowplot abbreviation")
        NA
      }
    )
  }
}

expand_cowplot_get <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "get"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "l" = "legend",
        "p" = "panel",
        "s" = "subtitle",
        "t" = "title",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown cowplot_get abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (length(out) == 3) {
    out[2] <- dplyr::case_when(
      x[2] == "x" ~ "x",
      x[2] == "y" ~ "y",
      x[2] == "p" ~ "panel"
    ) # get_plot_component
    out[3] <- ifelse(x[3] == "a", "axis", "component")
  }
  paste0(out, collapse = "_")
}

expand_cowplot_p <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "pb" = "panel_border",
    "pnd" = "pdf_null_device", # png_null_device
    "pcn" = "plot_component_names",
    "pc" = "plot_components",
    "pg" = "plot_grid",
    "ptg" = "plot_to_gtable",
    {
      message(paste(
        "second letter:",
        x[2],
        "unknown cowplot_p abbreviation"
      ))
      NA
    }
  )
}

expand_cowplot_draw <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 2) {
    out[1] <- "draw"
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "g" = "grob",
        "i" = "image", # png_null_device
        "l" = "label", # line
        "p" = "plot",
        "t" = "text",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown cowplot_draw abbreviation"
          ))
          NA
        }
      ), fixed = TRUE
    )
    return(paste0(out, collapse = "_"))
  } else if (length(out) == 3) {
    out <- ifelse(x[2] == "f", "draw_figure_label", "draw_plot_label")
  }
}

expand_cowplot_stamp <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("stamp")
  } else if (length(out) == 2) {
    out[1] <- "stamp"
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "b" = "bad",
        "g" = "good",
        "u" = "ugly",
        "w" = "wrong",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown cowplot_stamp abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    return(paste0(out, collapse = "_"))
  }
}

expand_cowplot_theme <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 2) {
    out[1] <- "theme"
    out[2] <- dplyr::case_when(
      x[2] == "m" ~ "map",
      x[2] == "c" ~ "cowplot",
      x[2] == "n" ~ "nothing"
    )
    return(paste0(out, collapse = "_"))
  } else if (length(out) == 3) {
    out[1] <- "theme"
    out[2] <- ifelse(x[2] == "m", "minimal", "half")
    out[3] <- dplyr::case_when(
      out[2] == "half" ~ "open",
      x[3] == "g" ~ "grid",
      x[3] == "h" ~ "hgrid",
      x[3] == "v" ~ "vgrid"
    )
    return(paste0(out, collapse = "_"))
  }
}

##########  ggsci ##########

#' Function to translate ggthemes package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggsci <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^p|r|s{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized themes starting chars:", ggsci_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggsci_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggsci_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 3)
  out <- character(length(x))
  switch(x[1],
    "p" = expand_ggsci_pal(x),
    "r" = {
      ifelse(x[2] == "g", "rgb_gsea", "rgb_material")
    },
    "s" = expand_ggsci_scale(x),
    {
      message("unknown ggsci abbreviation")
      NA
    }
  )
}

expand_ggsci_pal <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "pal"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "a" = "aaas",
      "d" = "d3",
      "f" = "futurama",
      "g" = "gsea",
      "i" = "igv",
      "j" = "jama", # jco
      "l" = "lancet", # locuszoom
      "m" = "material",
      "n" = "npg", # nejm
      "r" = "rickandmorty",
      "s" = "simpsons", # startrek
      "t" = "tron",
      "u" = "ucscgb", # uchicago
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown ggsci_pal abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_ggsci_scale <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "scale"
  if (x[2] == "c") {
    out[2] <- "colour"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "a" = "aaas",
        "d" = "d3",
        "f" = "futurama",
        "g" = "gsea",
        "i" = "igv",
        "j" = "jama", # jco
        "l" = "lancet", # locuszoom
        "m" = "material",
        "n" = "npg", # nejm
        "r" = "rickandmorty",
        "s" = "simpsons", # startrek
        "t" = "tron",
        "u" = "ucscgb", # uchicago
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown ggsci_scale abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (x[2] == "f") {
    out[2] <- "fill"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "a" = "aaas",
        "d" = "d3",
        "f" = "futurama",
        "g" = "gsea",
        "i" = "igv",
        "j" = "jama", # jco
        "l" = "lancet", # locuszoom
        "m" = "material",
        "n" = "npg", # nejm
        "r" = "rickandmorty",
        "s" = "simpsons", # startrek
        "t" = "tron",
        "u" = "ucscgb", # uchicago
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown ggsci_scale abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

##########  ggthemes ##########

#' Function to translate ggthemes package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggthemes <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^s|t|w|h||c|e|p{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized themes starting chars:", ggthemes_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggthemes_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

#' Function to transform ggthemes abbreviation into character string
#'
#' @param x string starting with r.
#' @return Adjusted cursor position in R script
expand_ggthemes_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 3)
  out <- character(length(x))
  switch(x[1],
    "s" = expand_ggthemes_scale(x),
    "t" = {
      if (length(out) == 3) {
        ifelse(x[2] == "c",
          "tableau_color_pal", "tableau_shape_pal"
        )
      } else if (length(out) == 4) {
        ifelse(x[2] == "d",
          "tableau_div_gradient_pal", "tableau_seq_gradient_pal"
        )
      } else {
        expand_ggthemes_theme(x)
      }
    },
    "w" = "wsj_pal",
    "h" = "hc_pal",
    "c" = "calc_pal",
    "e" = "economist_pal", # excel_pal
    "p" = "palette_pander",
    {
      message("unknown ggthemes abbreviation")
      NA
    }
  )
}

expand_ggthemes_theme <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "theme"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "b" = "base",
        "c" = "clean", # calc
        "e" = "economist", # excel
        "f" = "few", # fivethirtyeight foundation
        "g" = "gdocs",
        "h" = "hc",
        "i" = "igray",
        "m" = "map",
        "p" = "pander", # par
        "s" = "solarized", # solid stata
        "t" = "tufte",
        "w" = "wsj",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown ggthemes_theme abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (length(out) == 3) {
    out <- ifelse(x[2] == "e", "theme_economist_white", "theme_solarized_2")
  }
  paste0(out, collapse = "_")
}

expand_ggthemes_scale <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "scale"
  if (x[2] == "c") {
    out[2] <- "colour"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "c" = "colorblind",
        "e" = "economist", # excel
        "f" = "few", # fivethirtyeight foundation
        "g" = "gdocs",
        "h" = "hc",
        "i" = "igray",
        "m" = "map",
        "p" = "pander", # par
        "s" = "solarized", # solid stata
        "t" = "tableau",
        "w" = "wsj",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown ggthemes_scale abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (x[2] == "f") {
    out[2] <- "fill"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "c" = "colorblind",
        "e" = "economist", # excel
        "f" = "few", # fivethirtyeight foundation
        "g" = "gdocs",
        "h" = "hc",
        "p" = "pander", # par
        "s" = "solarized", # solid stata
        "t" = "tableau",
        "w" = "wsj",
        {
          message(paste(
            "third letter:",
            x[3],
            "unknown ggthemes_scale abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

##########  ggforce ##########
#' Function to translate ggforce package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggforce <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^f|g|l|n|p|r|s|t|i{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized ggforce starting chars:", ggforce_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggforce_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggforce_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 4)
  out <- character(length(x))
  switch(x[1],
    "f" = expand_ggforce_facet(x),
    "g" = expand_ggforce_geom(x),
    "l" = "linear_trans",
    "n" = "n_pages",
    "p" = {
      dplyr::case_when(
        x[2] == "a" ~ "position_auto",
        x[2] == "j" ~ "position_jitternormal",
        x[2] == "t" ~ "position_trans"
      )
    },
    "r" = "radial_trans",
    "s" = expand_ggforce_stat(x),
    "t" = {
      ifelse(x[2] == "r", "theme_reverser", "theme_no_axis")
    },
    "i" = "interpolateDataFrame", # weird name
    {
      message("unknown ggforce abbreviation")
      NA
    }
  )
}

expand_ggforce_facet <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "facet"
  if (length(out) == 2) {
    out[2] <- dplyr::case_when(
      x[2] == "c" ~ "col",
      x[2] == "m" ~ "matrix",
      x[2] == "r" ~ "row",
      x[2] == "s" ~ "stereo",
      x[2] == "z" ~ "zoom"
    )
  } else if (length(out) == 3) {
    out[2] <- ifelse(x[2] == "g", "grid", "wrap")
    out[3] <- "paginate"
  }
  paste0(out, collapse = "_")
}

expand_ggforce_geom <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "geom"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "a" = "arc", # arc0 arc2 autodensity autohistogram
        "b" = "bezier", # bspline bezier0 bezier2
        "c" = "circle",
        "d" = "diagonal",
        "e" = "ellipse",
        "l" = "link",
        "r" = "regon",
        "s" = "sina", # shape
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown ggforce_geom abbreviation"
          ))
          NA
        }
      )
    )
    return(paste0(out, collapse = "_"))
  } else if (length(out) == 3) {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "gab" = "geom_arc_bar", # arc0 arc2 autodensity autohistogram
      "gbc" = "geom_bspline_closed", # bspline bezier0 bezier2
      "gds" = "geom_delauney_segment",
      "gdt" = "geom_delauney_tile",
      "gdw" = "geom_diagonal_wide",
      "gmc" = "geom_mark_circle",
      "gme" = "geom_mark_ellipse",
      "gmh" = "geom_mark_hull",
      "gmr" = "geom_mark_rect",
      "gps" = "geom_parallel_sets",
      "gvs" = "geom_voronoi_segment",
      "gvt" = "geom_voronoi_title",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown ggforce_geom abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 4) {
    return(ifelse(x[4] == "a",
      "geom_parallel_sets_axes", "geom_parallel_sets_label"
    ))
  }
}

expand_ggforce_stat <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "stat"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "a" = "arc", # arc0 arc2 autodensity autohistogram
        "b" = "bezier", # bspline bezier0 bezier2
        "c" = "circle",
        "d" = "diagonal",
        "e" = "ellip",
        "l" = "link",
        "r" = "regon",
        "p" = "pie",
        "s" = "sina",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown ggforce_stat abbreviation"
          ))
          NA
        }
      )
    )
    return(paste0(out, collapse = "_"))
  } else if (length(out) == 3) {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "sab" = "stat_arc_bar", # arc0 arc2 autodensity autohistogram
      "sbc" = "stat_bspline_closed", # bspline bezier0 bezier2
      "sds" = "stat_delavor_summary",
      "sps" = "stat_parallel_sets",
      "sdw" = "set_diagonal_wide",
      {
        message("unknown ggforce_stat abbreviation")
        NA
      }
    )
  } else if (length(out) == 4) {
    return("stat_parallel_set_axes")
  }
}

##########  ggridges ##########

#' Function to translate ggridges package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggridges <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^c|g|p|s|t{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized ggridges starting chars:", ggridges_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggridges_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggridges_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  out <- character(length(x))
  switch(x[1],
    "c" = "cyclical_scale",
    "g" = expand_ggridges_geom(x),
    "p" = {
      dplyr::case_when(
        x[2] == "r" ~ "position_raincloud",
        x[3] == "j" ~ "position_points_jitter",
        x[3] == "s" ~ "position_points_sina"
      )
    },
    "s" = expand_ggridges_scale(x),
    "t" = "theme_ridges",
    {
      message("unknown ggridges abbreviation")
      NA
    }
  )
}

expand_ggridges_geom <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "geom"
  if (length(out) == 2) {
    out <- ifelse(x[2] == "r", "geom_ridgeline", "geom_vridgeline")
  } else if (length(out) == 3) {
    if (x[2] == "d") {
      out[2] <- "density"
      out[3] <- dplyr::case_when(
        x[3] == "l" ~ "lines",
        x[3] == "g" ~ "gradient",
        x[3] == "r" ~ "ridges"
      )
    } else if (x[2] == "r") {
      out[2] <- "ridgeline"
      out[3] <- "gradient"
    }
  } else if (length(out) == 4) {
    out <- "geom_density_ridges_gradient"
  }
  paste0(out, collapse = "_")
}

expand_ggridges_scale <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 3) {
    if (x[2] %in% c("v", "p")) {
      x <- stringi::stri_c(x, collapse = "")
      if (x == "svl") {
        return("scale_vline_linetype")
      }
      if (x == "sps") {
        return("scale_point_shape")
      }
    } else {
      out[1] <- "scale"
      out[3] <- "cyclical"
      out[2] <- sub(
        x = x[2],
        pattern = x[2],
        replacement = switch(x[2],
          "a" = "alpha",
          "c" = "colour",
          "f" = "fill",
          "l" = "linetype",
          "s" = "size",
          {
            message("unknown ggridges size 3 abbreviation")
            NA
          }
        )
      )
      return(paste0(out, collapse = "_"))
    }
  } else if (length(out) == 4) {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "spcc" = "scale_point_colour_continuous",
      "spcd" = "scale_point_colour_discrete",
      "spcg" = "scale_point_colour_gradient",
      "spch" = "scale_point_colour_hue",
      "spfc" = "scale_point_fill_continuous",
      "spfd" = "scale_point_fill_discrete",
      "spfg" = "scale_point_fill_gradient",
      "spfh" = "scale_point_fill_hue",
      "spsd" = "scale_point_shape_discrete",
      "spsc" = "scale_point_shape_continuous",
      "svcc" = "scale_vline_colour_continuous",
      "svcd" = "scale_vline_colour_discrete",
      "svcg" = "scale_vline_colour_gradient",
      "svch" = "scale_vline_colour_hue",
      "svld" = "scale_vline_linetype_discrete",
      "svsc" = "scale_vline_size_continuous",
      {
        message("unknown ggridges size 4 abbreviation")
        NA
      }
    )
  }
}

##########  ggraph ##########

#' Function to translate ggraph package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_ggraph <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^a|c|e|f|g|i|l|n|r|s|t|u{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized ggraph starting chars:", ggraph_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_ggraph_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_ggraph_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  out <- character(length(x))
  switch(x[1],
    "a" = "autograph",
    "c" = {
      dplyr::case_when(
        length(out) == 1 ~ "circle",
        length(out) == 2 & x[2] == "c" ~ "collect_connections",
        length(out) == 2 & x[2] == "e" ~ "collect_edges",
        length(out) == 2 & x[2] == "l" ~ "create_layout"
      )
    },
    "e" = {
      ifelse(length(x) == 2, "edge_angle", "ellipsis")
    },
    "f" = {
      dplyr::case_when(
        x[2] == "e" ~ "facet_edges",
        x[2] == "g" ~ "facet_graph",
        x[2] == "n" ~ "facet_nodes"
      )
    },
    "g" = expand_ggraph_geom(x),
    "i" = "is.geometry",
    "l" = {
      ifelse(length(x) == 2, "label_rect", "layout_to_table")
    },
    "n" = {
      ifelse(length(x) == 2, "node_angle", "node_rank_fabric")
    },
    "r" = "rectangle",
    "s" = {
      ifelse(length(x) == 1, "square", expand_ggraph_scale(x))
    },
    "t" = {
      dplyr::case_when(
        length(x) == 2 & x[2] == "f" ~ "th_foreground",
        length(x) == 2 & x[2] == "g" ~ "theme_graph",
        length(x) == 3 ~ "th_no_axes"
      )
    },
    "u" = "unset_graph_style",
    {
      message("unknown ggraph abbreviation")
      NA
    }
  )
}

expand_ggraph_geom <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "geom"
  if (x[2] == "a") {
    return("geom_axis_hive")
  }
  if (x[2] == "c") {
    return("geom_conn_bundle")
  }
  if (x[2] == "e") {
    out[2] <- "edge"
    out[3] <- sub(
      x = x[3],
      pattern = x[3],
      replacement = switch(x[3],
        "a" = "arc",
        "b" = "bend",
        "d" = "density", # diagonal
        "e" = "elbow",
        "f" = "fan",
        "l" = "link", # loop
        "p" = "parallel", # point
        "s" = "span",
        "t" = "tile",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown ggraph_geom abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  } else if (x[2] == "n") {
    if (length(out) == 4) {
      return("geom_node_arc_bar")
    } else {
      out[2] <- "node"
      out[3] <- sub(
        x = x[3],
        pattern = x[3],
        replacement = switch(x[3],
          "c" = "circle",
          "l" = "label",
          "p" = "point",
          "r" = "range",
          "t" = "text", # tile
          "v" = "voronoi",
          {
            message(paste(
              "third letter:",
              x[3],
              "unknown ggraph_geom abbreviation"
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

expand_ggraph_scale <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "scale"
  if (x[2] == "e") {
    if (length(out) == 3) {
      out[2] <- "edge"
      out[3] <- sub(
        x = x[3],
        pattern = x[3],
        replacement = switch(x[3],
          "a" = "alpha",
          "l" = "linetype",
          "r" = "radius",
          "s" = "shape", # size
          "w" = "width",
          {
            message(paste(
              "third letter:",
              x[3],
              "unknown ggraph_scale abbreviation"
            ))
            NA
          }
        ),
        fixed = TRUE
      )
      return(paste0(out, collapse = "_"))
    } else {
      x <- stringi::stri_c(x, collapse = "")
      switch(x,
        "seac" = "scale_edge_alpha_continuous",
        "sead" = "scale_edge_alpha_discrete",
        "seai" = "scale_edge_alpha_identity",
        "seam" = "scale_edge_alpha_manual",
        "secc" = "scale_edge_colour_continuous",
        "secd" = "scale_edge_colour_discrete",
        "seci" = "scale_edge_colour_identity",
        "secm" = "scale_edge_colour_manual",
        "secb" = "scale_edge_colour_brewer",
        "secg" = "scale_edge_colour_gradient", # grey
        "sech" = "scale_edge_colour_hue",
        "secv" = "scale_edge_colour_viridis",
        "sefc" = "scale_edge_fill_continuous",
        "sefd" = "scale_edge_fill_discrete",
        "sefi" = "scale_edge_fill_identity",
        "sefm" = "scale_edge_fill_manual",
        "sefb" = "scale_edge_fill_brewer",
        "sefg" = "scale_edge_fill_gradient", # grey
        "sefh" = "scale_edge_fill_hue",
        "sefv" = "scale_edge_fill_viridis",
        "selc" = "scale_edge_linetype_continuous",
        "seld" = "scale_edge_linetype_discrete",
        "seli" = "scale_edge_linetype_identity",
        "selm" = "scale_edge_linetype_manual",
        "selb" = "scale_edge_linetype_brewer",
        "selg" = "scale_edge_linetype_gradient", # grey
        "selh" = "scale_edge_linetype_hue",
        "selv" = "scale_edge_linetype_viridis",
        "sesc" = "scale_edge_shape_continuous",
        "sesd" = "scale_edge_shape_discrete",
        "sesi" = "scale_edge_shape_identity",
        "sesm" = "scale_edge_shape_manual",
        "sewc" = "scale_edge_width_continuous",
        "sewd" = "scale_edge_width_discrete",
        "sewi" = "scale_edge_width_identity",
        "sewm" = "scale_edge_width_manual",
        {
          message("unknown ggraph_geom abbreviation")
          NA
        }
      )
    }
  } else if (x[2] == "l") {
    if (length(out) == 3) {
      return("scale_label_size")
    }
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "slsc" = "scale_label_size_continuous",
      "slsd" = "scale_label_size__discrete",
      "slsi" = "scale_label_size_identity",
      "slsm" = "scale_label_size__manual",
      {
        message("unknown ggraph_geom abbreviation")
        NA
      }
    )
  }
}

##########  scales ##########

#' Function to translate scales package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
# #' @export
construct_scales <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (grepl("^j|k|l{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized scales starting chars:", scales_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "+") {
    warning("+ operator mode spotted while making plot.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_scales_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_scales_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  out <- character(length(x))
  switch(x[1],
    "a" = {
      dplyr::case_when(
        length(out) == 1 ~ "alpha",
        x[2] == "a" ~ "abs_area",
        x[2] == "p" ~ "area_pal",
        x[2] == "t" ~ "area_trans"
      ) # asn_trans atanh_trans
    },
    "b" = {
      x <- stringi::stri_c(x, collapse = "")
      switch(x,
        "bt" = "boxcox_trans",
        "be" = "breaks_extended",
        "bl" = "breaks_log",
        "bp" = "brewer_pal", # breaks_pretty
        "bw" = "breaks_with",
        {
          message("unknown scales_b abbreviation")
          NA
        }
      )
    },
    "c" = expand_scales_c(x),
    "d" = expand_scales_d(x),
    "e" = {
      dplyr::case_when(
        x[2] == "r" ~ "expand_range",
        x[2] == "b" ~ "extended_breaks",
        x[2] == "t" ~ "exp_trans"
      )
    },
    "f" = "fullseq",
    "g" = ifelse(length(out) == 2, "grey_pal", "gradient_n_pal"),
    "h" = ifelse(x[2] == "t", "hms_trans", "hue_pal"),
    "i" = ifelse(x[2] == "t", "identity_trans", "identity_pal"), # is.trans
    "l" = expand_scales_label(x),
    "m" = expand_scales_m(x),
    "n" = {
      dplyr::case_when(
        length(out) == 1 ~ "number",
        length(out) == 3 ~ "number_bytes_format",
        x[2] == "f" ~ "number_format",
        length(out) == 2 & x[2] == "b" ~ "number_bytes"
      )
    },
    "o" = expand_scales_o(x),
    "p" = expand_scales_p(x),
    "r" = expand_scales_r(x),
    "s" = expand_scales_s(x),
    "t" = expand_scales_t(x),
    "u" = "unit_format",
    "v" = "viridis_pal",
    "w" = "wrap_format",
    "y" = "yj_trans",
    "z" = "zero_range",
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown scales abbreviation"
      ))
      NA
    }
  )
}

expand_scales_c <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("comma") # cbreaks censor col2hcl cscale
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "cb" = "col_bin",
      "cf" = "col_factor",
      "cn" = "col_numeric",
      "cq" = "col_quantile",
      "cr" = "colour_ramp",
      {
        message("unknown scales_c abbreviation")
        NA
      }
    )
  }
}

expand_scales_d <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 3) {
    return("div_gradient_pal") # cbreaks censor col2hcl cscale
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "db" = "date_breaks",
      "df" = "date_format", # dollar_format
      "dt" = "date_trans",
      "dp" = "dichromat_pal",
      "d" = "dollar", # discrete dscale
      {
        message("unknown scales_d abbreviation")
        NA
      }
    )
  }
}

expand_scales_label <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "lb" = "log_breaks", # label_bytes
    "lc" = "label_comma",
    "ld" = "label_date", # label_dollar
    "lds" = "label_date_short",
    "lm" = "label_math",
    "ln" = "label_number",
    "lna" = "label_number_auto",
    "lns" = "label_number_si",
    "lo" = "label_ordinal",
    "lp" = "label_percent", # label_parse pvalue linetype_pal
    "ls" = "label_scientific",
    "lt" = "label_time", # log_trans
    "lw" = "label_wrap",
    {
      message("unknown scales_label abbreviation")
      NA
    }
  )
}

expand_scales_m <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  dplyr::case_when(
    length(out) == 1 ~ "muted",
    length(out) == 3 & x[3] == "w" ~ "minor_breaks_with",
    length(out) == 3 & x[3] == "n" ~ "minor_breaks_n",
    x[2] == "p" ~ "manual_pal",
    x[2] == "f" ~ "math_format",
    x[2] == "t" ~ "modulus_trans"
  )
}

expand_scales_o <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("ordinal")
  } else if (length(out) == 3) {
    dplyr::case_when(
      x[2] == "s" & x[3] == "a" ~ "oob_squish_any",
      x[2] == "s" & x[3] == "i" ~ "oob_squish_infinite",
      x[2] == "c" ~ "oob_censor_any"
    )
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "oc" = "oob_censor",
      "ob" = "oob_discard",
      "ok" = "oob_keep",
      "os" = "oob_squish", # ordinal_spanish
      "oe" = "ordinal_english",
      "of" = "ordinal_format", # ordinal_french (subset of ordinal_format)
      {
        message("unknown scales_o abbreviation")
        NA
      }
    )
  }
}

expand_scales_p <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("percent") # pvalue
  } else if (length(out) == 3) {
    return("pseudo_log_trans")
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "pf" = "percent_format", # pvalue_format
      "pb" = "pretty_breaks",
      "pt" = "probability_trans", # probit_trans
      {
        message("unknown scales_p abbreviation")
        NA
      }
    )
  }
}

expand_scales_r <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("rescale")
  } else if (length(out) == 3) {
    return("regular_minor_breaks")
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "rt" = "reciprocal_trans", # pvalue_format reverse_trans
      "rm" = "rescale_max", # rescale_mid
      "rn" = "rescale_none",
      "rp" = "rescale_pal",
      {
        message("unknown scales_r abbreviation")
        NA
      }
    )
  }
}

expand_scales_s <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  if (length(out) == 1) {
    return("scientific") # sqrt
  } else if (length(out) == 3) {
    return("seq_gradient_pal")
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "sf" = "scientific_format",
      "sp" = "shape_pal",
      "sc" = "show_col",
      "st" = "sqrt_trans",
      "si" = "squish_infinite",
      {
        message("unknown scales_s abbreviation")
        NA
      }
    )
  }
}

expand_scales_t <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "tf" = "time_format", # trans_format
    "tt" = "time_trans",
    "tc" = "train_continuous",
    "td" = "train_discrete",
    "tb" = "train_breaks",
    "tn" = "trans_new",
    "tr" = "trans_range",
    {
      message("unknown scales_t abbreviation")
      NA
    }
  )
}
