# DT


##########  shiny ##########

#' Function to insert shiny shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_shiny <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL,
    Sys.info()["user"]
  )$res
  ### TODO:  if nchar is 1, make shortcut of widely used functions
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (grepl("^x|y|z|j|z{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized shinystarting chars:", shiny_error))
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
  expression <- expand_shiny_default(abb)
  if (grepl("na", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_shiny_default <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  out <- switch(x[1],
    "g" = expand_shiny_get(x),
    "h" = expand_shiny_h(x),
    "i" = expand_shiny_i(x),
    "m" = expand_shiny_m(x),
    "r" = expand_shiny_r(x),
    "s" = expand_shiny_s(x),
    "t" = expand_shiny_t(x),
    "u" = expand_shiny_u(x),
    "v" = expand_shiny_v(x),
    "w" = expand_shiny_w(x),
    "a" = expand_shiny_a(x),
    "b" = expand_shiny_b(x),
    "c" = expand_shiny_c(x),
    "o" = expand_shiny_o(x),
    "d" = expand_shiny_d(x),
    "e" = expand_shiny_e(x),
    "f" = expand_shiny_f(x),
    "n" = expand_shiny_n(x),
    "p" = expand_shiny_p(x),
    "k" = "key_missing",
    "l" = "load_support",
    "q" = "quo_to_function",
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown shiny abbreviation"
      ))
      NA
    }
  )
  ### TODO:check this
  snakecase::to_lower_camel_case(out)
}

expand_shiny_a <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("a")
  } else if (length(out) == 2) {
    switch(x,
      "ap" = "absolute_panel",
      "ab" = "action_button",
      "al" = "action_link",
      "ao" = "animation_options",
      "at" = "append_tab",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_a abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    ifelse(x == "asa", "as_shiny_appobj", "add_resource_path")
  }
}

expand_shiny_b <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("br")
  } else if (length(out) == 2) {
    switch(x,
      "bp" = "basic_page",
      "bc" = "bind_cache",
      "be" = "bind_event",
      "bb" = "bookmark_button",
      "bl" = "bootstrap_lib",
      # "bp" = "bootstrap_page", #brushed_points
      "bv" = "browser_viewer",
      "bo" = "brush_opts",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_b abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_c <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("column") # code (imported)
  } else if (length(out) == 2) {
    switch(x,
      "cm" = "call_module",
      "ci" = "checkbox_input",
      "co" = "click_opts",
      "cp" = "conditional_panel",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_c abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "cst" = "capture_stack_traces", # condition_stack_trace
      "cgi" = "checkbox_group_input",
      "crf" = "create_render_function",
      "cwd" = "create_web_dependency",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_c abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_d <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("debounce") # dev(i), devmode
  } else if (length(out) == 2) {
    switch(x,
      "di" = "date_input",
      "do" = "dblclick_opts",
      "dv" = "dialog_viewer",
      "dc" = "disk_cache",
      "db" = "download_button",
      "dh" = "download_handler",
      "dl" = "download_link",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_d abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "dto" = "data_table_output",
      "dri" = "date_range_input",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_d abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_e <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("em") # dev(i), devmode
  } else if (length(out) == 2) {
    switch(x,
      "eb" = "enable_bookmarking",
      "er" = "event_reactive",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_e abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "etv" = "export_test_values",
      "etf" = "expr_to_function",
      "est" = "extract_stack_trace",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_e abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_f <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 2) {
    switch(x,
      "fi" = "file_input",
      "fc" = "fluid_col",
      "fr" = "fluid_row", # fill_row, fixed_row
      "fp" = "fluid_page", # fill_page, fixed_page, fixed_panel
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_d abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "fst" = "format_stack_trace",
      "frv" = "freeze_reactive_value",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_f abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_get <- function(x) {
  stopifnot(length(x) >= 3 & length(x) < 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "gcoi" = "get_current_output_info",
    "gct" = "get_current_theme",
    "gdrd" = "get_default_reactive_domain",
    "gdo" = "get_devmode_option",
    "gqs" = "get_query_string",
    "gso" = "get_shiny_option",
    "guh" = "get_url_hash",
    {
      message(paste(
        "unknown shiny_get abbreviation"
      ))
      NA
    }
  )
}

expand_shiny_h <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "ht" = "help_text", # hide_tab html_template
    "ho" = "hover_opts", # html_output
    "h" = "html",
    "hr" = "html_response",
    {
      message(paste(
        "unknown shiny_h abbreviation"
      ))
      NA
    }
  )
}

expand_shiny_i <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("img")
  } else if (length(out) == 2) {
    switch(x,
      "id" = "in_devmode",
      "ip" = "inc_progress", # input_panel
      "ic" = "include_css",
      "ih" = "include_html",
      "im" = "include_markdown",
      "is" = "include_script", # is_singleton
      "it" = "include_text", # insert_tab, is_truthy
      "il" = "invalidate_later",
      "iu" = "insert_ui",
      "ir" = "is.reactivevalues", # is_reactive, is_running
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_i abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "isa" = "is_shiny_appobj",
      "ief" = "install_expr_function",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_i abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_m <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("markdown")
  } else if (length(out) == 2) {
    switch(x,
      "mp" = "main_panel",
      "mc" = "memory_cache", # input_panel
      "mb" = "modal_button",
      "md" = "modal_dialog",
      "ms" = "module_server",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_m abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "mrb" = "make_reactive_binding",
      "mrf" = "mark_render_function",
      "mrc" = "mask_reactive_context",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_m abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_n <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("need") # NS
  } else if (length(out) == 2) {
    switch(x,
      "nm" = "navbar_menu",
      "np" = "navbar_page", # navlist_panel, #near_points
      "ni" = "numeric_input",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_n abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_o <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("observe")
  } else {
    switch(x,
      "oe" = "observe_event",
      "ob" = "on_bookmarked", # on_bookmark
      "of" = "on_flushed", # on_flush
      "or" = "on_restored", # on_restore
      "os" = "on_stop",
      "ose" = "on_session_ended",
      "oo" = "output_options",
      "orde" = "on_reactive_domain_ended",
      {
        message(paste(
          "unknown shiny_o abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_p <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("pre") # p(i)
  } else if (length(out) == 2) {
    switch(x,
      "pv" = "pane_viewer",
      "pi" = "password_input",
      "po" = "plot_output",
      "pp" = "plot_png",
      "pt" = "prepend_tab",
      "pe" = "print_error",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_p abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "pws" = "page_with_sidebar",
      "pqs" = "parse_query_string",
      "pst" = "print_stack_trace",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_p abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_r <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("reactive") # repeatable, req, reactlog
  } else if (length(out) == 2) {
    switch(x,
      "rb" = "radio_buttons",
      "rc" = "reactive_console",
      "rp" = "reactive_plot", # reactive_poll, reactive_print, remove_plot, remove_print, resource_paths
      "rt" = "reactive_text", # reactive_table, reactive_timer, #remove_tab, render table, render_text, run_tests
      "ru" = "reactive_ui", # remove_ui, render_ui, "run_url
      "rv" = "reactive_values", # reactive_val,
      "rr" = "reactlog_reset",
      "rs" = "reactlog_show",
      "rm" = "remove_modal",
      "rn" = "remove_notification",
      "ri" = "remove_image", # restore_input
      "rg" = "run_gist", # run_gadget
      "ra" = "run_app",
      "re" = "run_example",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_r abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "rfr" = "reactive_file_reader",
      "rih" = "register_input_handler",
      "rtd" = "register_theme_dependency",
      "rrp" = "remove_resource_path",
      "rcp" = "remove_cached_plot",
      "rdt" = "render_data_table",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_r abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_s <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("singleton") # span(i)
  } else if (length(out) == 2) {
    switch(x,
      "se" = "safe_error", # snapshot_exclude
      "si" = "selectize_input", # select_input, server_info, slider_input
      "sp" = "sidebar_penal", # set_progress
      "ss" = "set_serializer",
      "sa" = "stop_app", # shiny_app
      "so" = "shiny_options",
      "su" = "shiny_ui",
      "sm" = "show_modal",
      "sn" = "show_notification",
      "st" = "show_tab",
      "sl" = "sidebar_layout", # split_layout
      "sb" = "submit_button",
      "sd" = "suppress_dependencies",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_s abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "sbe" = "set_bookmark_exclude",
      "sat" = "shiny_app_template",
      "srl" = "show_react_log",
      "sgr" = "size_growth_ratio",
      "spi" = "snapshot_preprocess_input",
      "spo" = "snapshot_preprocess_output",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_s abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_t <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("throttle") # tag
  } else if (length(out) == 2) {
    switch(x,
      "tl" = "tag_list",
      "ts" = "test_server",
      "ti" = "text_input",
      "to" = "text_output",
      "tp" = "title_panel",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_t abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "taa" = "tag_append_attributes",
      "tac" = "tag_append_children", # tag_append_child
      "tga" = "tag_get_attribute",
      "tha" = "tag_has_attribute",
      "tsc" = "tag_set_children",
      "tai" = "text_area_input",
      {
        message(paste(
          "three letters:",
          x,
          "unknown shiny_t abbreviation"
        ))
        NA
      }
    )
  }
}

expand_shiny_u <- function(x) {
  stopifnot(length(x) >= 3 & length(x) < 5)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "uab" = "update_action_button",
    "ual" = "update_action_link",
    "ucgp" = "update_checkbox_group_input",
    "uci" = "update_checkbox_input",
    "unp" = "update_navbar_page", # update_navlist_panel
    "uni" = "update_numeric_input",
    "uqs" = "update_query_string",
    "urb" = "update_radio_buttons",
    "usi" = "update_selectize_input", # update_select_input, update_slider_input
    "utp" = "update_tabset_panel",
    "uti" = "update_text_input",
    "utai" = "update_text_area_input",
    "uvsi" = "update_var_selectize_input", # update_var_select_input
    {
      message(paste(
        "two letters:",
        x,
        "unknown shiny_u abbreviation"
      ))
      NA
    }
  )
}

expand_shiny_v <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 1) {
    return("validate")
  } else if (length(out == 2)) {
    return("vertical_layout")
  } else if (length(out) == 3) {
    out <- switch(x,
      "vcu" = "validate_css_unit",
      "vsi" = "var_selectize_input", # var_Select_input
      "vto" = "verbatim_text_output",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_v abbreviation"
        ))
        NA
      }
    )
  }
  out
}

expand_shiny_w <- function(x) {
  stopifnot(length(x) >= 1 & length(x) < 5)
  out <- character(length(x))
  x <- stringi::stri_c(x, collapse = "")
  if (length(out) == 2) {
    switch(x,
      "wp" = "with_progress", # well_panel
      "wd" = "with_devmode",
      "wt" = "with_tags",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_w abbreviation"
        ))
        NA
      }
    )
  } else if (length(out) == 3) {
    switch(x,
      "wle" = "with_log_errors",
      "wmj" = "with_math_jax", # var_Select_input
      "wrd" = "with_reactive_domain",
      {
        message(paste(
          "two letters:",
          x,
          "unknown shiny_w abbreviation"
        ))
        NA
      }
    )
  } else {
    message("unanticipated case")
    return("NA")
  }
}
