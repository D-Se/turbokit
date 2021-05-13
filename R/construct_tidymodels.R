##########  recipes##########
#' Function to translate recipe package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_recipes <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^r|b|j|p|a|s|c|h|u{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized recipe starting chars:", recipes_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "%>%") {
    warning("+ operator mode spotted while making recipe.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_recipes_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_recipes_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 3)
  switch(x[1],
    "r" = "recipe", # remove_role?
    "b" = "bake",
    "j" = "juice",
    "p" = "prep",
    "a" = expand_recipes_all(x), # add?
    "s" = expand_recipes_step(x),
    "c" = expand_recipes_check(x),
    "h" = expand_recipes_has(x),
    "u" = expand_recipes_update(x),
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown recipe abbreviation"
      ))
      NA
    }
  )
}

expand_recipes_all <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "all"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "n" = "nominal",
      "o" = "outcomes",
      "p" = "predictors",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown recipe_all abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_recipes_step <- function(x) {
  stopifnot(length(x) <= 4)
  out <- character(length(x))
  out[1] <- "step"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "a" = "arrange",
      "b" = "BoxCox", # bagimpute, bin2factor, bs
      "c" = "center", # classdist, corr, count, cut
      "d" = "dummy", # discretize, downsample
      "f" = "filter", # factor2string
      "g" = "geodist",
      "h" = "holiday", # hyperbolic
      "i" = "interact", # intercept, integer, inverse, invlogit, isomap
      "k" = "kpca", # knnimpute
      "l" = "lincomb", # lag, log, logit
      "n" = "normalize",
      "o" = "other",
      "p" = "pls",
      "r" = "regex",
      "s" = "scale",
      "u" = "unknown",
      "w" = "window",
      "y" = "YeoJohnson",
      "z" = "zv",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown recipe_step abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  if (length(out) == 3) {
    if (x[3] == "p" | x[3] == "r") {
      out[3] <- ifelse(x[3] == "p", "poly", "rbf")
    } else if (x[3] == "l") {
      out[3] <- "linear"
      out[2] <- "impute"
    }
  }
  paste0(out, collapse = "_")
}

expand_recipes_check <- function(x) {
  stopifnot(length(x) <= 3)
  out <- character(length(x))
  out[1] <- "check"
  if (length(out) == 3) {
    out[2] <- "new"
    out[3] <- "values"
  } else {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "c" = "class",
        "m" = "missing",
        "n" = "name",
        "r" = "range", # jitterdodge?
        "t" = "type",
        {
          message(paste(
            "second letter:",
            x[2],
            "unknown recipe_check abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  paste0(out, collapse = "_")
}

expand_recipes_has <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "has"
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "t" = "type",
      "r" = "role",
      {
        message(paste(
          "second letter:",
          x[2],
          "unknown recipe_has abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  paste0(out, collapse = "_")
}

expand_recipes_update <- function(x) {
  stopifnot(length(x) <= 2)
  out <- character(length(x))
  out[1] <- "update"
  if (length(out) == 2) {
    out[2] <- "role"
  }
  paste0(out, collapse = "_")
}

##########  parsnip ##########
#' Function to translate parsnip package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_parsnip <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^d|b|c|e|g{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized parsnip starting chars:", parsnip_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "%>%") {
    warning("+ operator mode spotted while making recipe.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_parsnip_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_parsnip_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 3)
  out <- character(length(x))
  if (x[1] == "g") {
    return(expand_parsnip_get(x))
  }
  if (length(out) == 1) {
    out[1] <- sub(
      x = x[1],
      pattern = x[1],
      replacement = switch(x[1],
        "a" = "augment",
        "t" = "translate",
        "v" = "varying",
        "f" = "fit",
        "m" = "mars",
        {
          message(paste(
            "first letter:",
            x[1],
            "unknown parsnip abbreviation"
          ))
          NA
        }
      )
    )
    return(paste0(out, collapse = "_"))
  } else if (length(out) == 2) {
    x <- stringi::stri_c(x, collapse = "")
    out <- sub(
      x = x,
      pattern = x,
      replacement = switch(x,
        "dt" = "decision_tree", # remove_role?
        "bt" = "boost_tree",
        "ct" = "C5.0_train",
        "ea" = "eval_args",
        "ar" = "add_rowindex",
        "fc" = "fit_control",
        "fx" = "fit_xy",
        "iv" = "is_varying",
        "km" = "keras_mlp",
        "lr" = "linear_reg", # logistic_reg
        "mc" = "min_cols", # make_call
        "mr" = "multinum_reg", # min_rows
        "mp" = "multi_predict", # model_printer
        "mm" = "maybe_matrix",
        "nn" = "nearest_neighbor",
        "nm" = "null_model",
        "nv" = "null_value",
        "pa" = "parsnip_addin",
        "pr" = "predict_raw",
        "pd" = "predict_data",
        "rf" = "rand_forest",
        "rc" = "repair_call",
        "rp" = "req_pkgs",
        "rt" = "rpart_train",
        "sa" = "set_args",
        "sd" = "set_dependency",
        "se" = "set_engine", # set_encoding show_engines
        "sf" = "set_fit", # shot_fit
        "sm" = "set_mode",
        "sp" = "set_predict", # svm_poly
        "sc" = "show_call",
        "sr" = "surv_reg", # svm_rbf
        "xt" = "xgb_train",
        "va" = "varying_args",
        {
          message(paste(
            "unknown parsnip abbreviation"
          ))
          NA
        }
      )
    )
  } else if (length(out) == 3) {
    x <- stringi::stri_c(x, collapse = "")
    out <- sub(
      x = x,
      pattern = x,
      replacement = switch(x,
        "cee" = "check_empty_ellipse",
        "cfp" = "check_final_param",
        "coh" = "control_one_hot",
        "csi" = "convert_stan_interval",
        "gfe" = "get_from_env",
        "gme" = "get_model_env",
        "gpt" = "get_predict_type",
        "hmp" = "has_multi_predict",
        "mpa" = "multi_predict_args",
        "pvt" = "pred_value_template",
        "udc" = "update_dot_check",
        "mdf" = "maybe_data_frame",
        "sci" = "stan_conf_int",
        "smi" = "show_model_info",
        "sme" = "set_model_engine",
        "smm" = "set_model_mode",
        "sie" = "set_in_env",
        "sma" = "set_model_arg",
        {
          message(paste(
            "unknown parsnip abbreviation"
          ))
          NA
        }
      )
    )
  }
}

expand_parsnip_get <- function(x) {
  stopifnot(length(x) <= 4)
  out <- character(length(x))
  out[1] <- "get"
  if (length(out) == 2) {
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "d" = "dependency",
        "e" = "encoding",
        "f" = "fit",
        {
          message(paste(
            "second letter:",
            x[1],
            "unknown parsnip_get abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
    paste0(out, collapse = "_")
  } else if (length(out) == 3) {
    out[2] <- dplyr::case_when(
      x[2] == "f" ~ "from",
      x[2] == "m" ~ "model",
      x[2] == "p" ~ "pred"
    )
    out[3] <- ifelse(x[3] == "e", "env", "type")
  }
  paste0(out, collapse = "_")
}
### TODO: Needs thorough check for naming policy

##########  tune ##########
#' Function to insert tune shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_tune <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^c|e|f|l|m|o|p|r|s|t{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter, currently recognized
                    tune starting chars:", tune_error))
    return(NULL)
  } else if (nchar(abb) > 6) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "%>%") {
    if (check_plot_context()) invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_tune_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_tune_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  ### TODO: order of appearance here?
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "t" = "tune",
    "ta" = "tune_args",
    "tb" = "tune_bayes",
    "tg" = "tune_grid",
    "sb" = "show_best",
    "cm" = "collect_metrics",
    "cp" = "collect_predictions",
    "cb" = "control_bayes", # conf_bound,
    "cg" = "control_grid",
    "cmf" = "conf_mat_resampled",
    "cr" = "control_resamples",
    "cop" = "coord_ops_pred",
    "ei" = "exp_improve",
    "ed" = "expo_decay",
    "em" = "extract_model",
    "er" = "extract_recipe",
    "fp" = "filter_parameters",
    "fm" = "finalize_model",
    "fr" = "finalize_recipe", # fit_resamples
    "fw" = "finalize_workflow",
    "fmv" = "fit_max_value",
    "lf" = "last_fit",
    "lp" = "load_pkgs",
    "mw" = "message_wrap",
    "on" = "outcome_names",
    "pi" = "prob_improve",
    "sbose" = "select_by_one_std_error",
    "sbope" = "select_by_one_pct_loss",
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

##########  dials ##########

#' Function to insert dials shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_dials <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (grepl("^j|q|x|y|z{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter, currently recognized
                    dials starting chars:", dials_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (!pipe_toggle()$pipe == "%>%") {
    if (check_plot_context()) invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_dials_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_dials_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 6)
  ### TODO: order of appearance here?
  out <- character(length = length(x))
  if (length(out) >= 2 & grepl(x = x[1], pattern = "^g|n")) {
    out <- sub(
      x = x[1],
      pattern = x[1],
      replacement = switch(x[1],
        "g" = expand_dials_g(x),
        "n" = expand_dials_num(x),
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
  } else {
    ### TODO:sequence of appearance here. Split in half with grepl?
    x <- stringi::stri_c(x, collapse = "")
    out <- sub(
      x = x,
      pattern = x,
      replacement = switch(x,
        "a" = "activation",
        "an" = "all_neighbors",
        "bs" = "batch_size",
        "c" = "cost",
        "cs" = "cost_complexity",
        "cf" = "confidence_factor",
        "df" = "deg_free",
        "d" = "dropout", # degree
        "di" = "degree_int",
        "dp" = "dist_power",
        "d" = "dropout",
        "eu" = "encode_unit",
        "e" = "epochs", # extrapolation
        "f" = "finalize",
        "fc" = "freq_cut",
        "ft" = "fuzzy_thresholding",
        "hu" = "hidden_units", # has_unknowns
        "iu" = "is_unknown",
        "ko" = "kernell_offset",
        "l" = "Laplace",
        "lr" = "learn_rate", # loss_reduction
        "lq" = "lower_quantile",
        "n" = "neighbors",
        "m" = "mtry", # mixture
        "or" = "over_ratio",
        "p" = "parameters", # penalty
        "ps" = "param_set",
        "pc" = "parameters_constr",
        "pp" = "predictor_prop",
        "pw" = "predictor_winnowing",
        "pd" = "prod_degree",
        "pm" = "prune_method",
        "pdo" = "pull_dials_object",
        "rg" = "range_get",
        "rs" = "range_set", # rbf_sigma
        "rv" = "range_validate",
        "rf" = "regularization_factor",
        "rd" = "regularization_depth",
        "rb" = "rule_bands",
        "sp" = "sample_prop",
        "sz" = "sample_size",
        "sf" = "scale_factor",
        "sh" = "signed_hash",
        "st" = "significance_threshold",
        "s" = "smoothness",
        "sd" = "spline_degree",
        "sr" = "splitting_rule",
        "sm" = "svm_margin",
        "t" = "threshold", # trees #tokens
        "td" = "tree_depth",
        "u" = "unknown",
        "uc" = "unique_cut",
        "ur" = "under_ratio", # unbiased_rules
        "vs" = "value_sample", # value_set, value_seq
        "vi" = "value_inverse",
        "vt" = "value_transform",
        "vv" = "value_validate",
        "w" = "weight",
        "wf" = "weight_func",
        "ws" = "weight_scheme", # window_size
        "mu" = "min_unique",
        "mn" = "max_nodes", # min_n
        "mnt" = "max_num_terms",
        "md" = "min_dist",
        "mr" = "max_rules",
        "ml" = "mtry_long",
        {
          message(paste(
            "first letter:",
            x[1],
            "unknown ggplot abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    )
  }
  out
}

expand_dials_g <- function(x) {
  out <- character(length(x))
  if (length(out) == 2) {
    return(dplyr::case_when(
      x[2] == "r" ~ "grid_random",
      x[2] == "n" ~ "get_n",
      x[2] == "p" ~ "get_p"
    ))
  } else if (x[3] == "h" | x[3] == "e") {
    if (length(out) == 3) {
      if (x[2] == "m") {
        out <- "grid_max_entropy"
      }
      if (x[2] == "l") {
        out <- "grid_latin_hypercube"
      }
    }
    return(out)
  } else {
    out[1] <- "get"
    if (length(out) == 3) {
      out[3] <- sub(
        x = x[3],
        pattern = x[3],
        replacement = switch(x[3],
          "s" = "sizes",
          "r" = "range",
          "f" = "frac",
          "p" = "p",
          {
            message(paste(
              "third letter:",
              x[3],
              "unknown dials abbreviation"
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
          "b" = "batch",
          "l" = "log",
          "n" = "n",
          "r" = "rbf",
          "p" = "p",
          {
            message(paste(
              "third letter:",
              x[3],
              "unknown dials abbreviation"
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
          "n" = "n",
          "p" = "p",
          {
            message(paste(
              "third letter:",
              x[3],
              "unknown dials abbreviation"
            ))
            NA
          }
        ),
        fixed = TRUE
      )
    } else if (length(out) == 4) {
      return("get_n_frac_range")
    }
    paste0(out, collapse = "_")
  }
}

expand_dials_num <- function(x) {
  out <- character(length(x))
  if (length(out) == 3) {
    if (x[2] == "r") {
      out <- "num_random_splits"
    } else {
      out <- "no_global_pruning"
    }
  } else {
    out[1] <- "num"
    out[2] <- sub(
      x = x[2],
      pattern = x[2],
      replacement = switch(x[2],
        "b" = "breaks",
        "c" = "comp",
        "h" = "hash",
        "t" = "terms", # tokens
        {
          message(paste(
            "first letter:",
            x[1],
            "unknown ggplot abbreviation"
          ))
          NA
        }
      ),
      fixed = TRUE
    ) # tokens
    paste0(out, collapse = "_")
  }
}

##########  yardstick ##########

#' Function to insert yardstick shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_yardstick <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^e|k|l|o|q|u|w|x|y|z {1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized yardstick starting chars:", yardstick_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_yardstick_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

### TODO:yarstick uses many abbreviation as function names - contact authors?
expand_yardstick_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 4)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "a" = "accuracy",
    "av" = "accuracy_vec",
    "ap" = "average_precision",
    "apv" = "average_precision_vec",
    "ba" = "bal_accuracy",
    "bav" = "bal_accuracy_vec",
    "c" = "ccc",
    "cv" = "ccc_vec",
    "cc" = "classification_cost",
    "ccv" = "classification_cost_vec",
    "cm" = "conf_mat",
    "dt" = "detection_prevalence",
    "dpv" = "detection_prevalence_vec",
    "dte" = "dots_to_estimate",
    "fm" = "f_meas",
    "fmv" = "f_meas_vec",
    "fe" = "finalize_estimator",
    "fei" = "finalize_estimator_internal",
    "gc" = "gain_curve", # gain_capture (similar to lift_curve)
    "gcv" = "gain_capture_vec",
    "gw" = "get_weights",
    "hl" = "huber_loss",
    "hlp" = "huber_loss_pseudo",
    "hlpv" = "huber_loss_pseudo_vec",
    "hlv" = "huber_loss_vec",
    "i" = "iic",
    "iv" = "iic_vec",
    "ji" = "j_index",
    "jiv" = "j_index_vec",
    "k" = "kap",
    "kv" = "kap_vec",
    "lf" = "lift_curve",
    "m" = "metrics", # mae, mase, mcc, msd, mape, mse
    "ms" = "metric_set", # metric_summarizer
    "mt" = "metric_tweak",
    "mvt" = "metric_vec_template",
    "mll" = "mn_log_loss",
    "mllv" = "mn_log_loss_vec",
    "ncm" = "new_class_metric",
    "nnm" = "new_numeric_metric",
    "npm" = "new_prob_metric",
    "n" = "npv",
    "nv" = "npv_vec",
    "p" = "precision", # ppv
    "pv" = "precision_vec", # ppv_vec
    "pa" = "pr_auc",
    "pav" = "pr_auc_vec",
    "r" = "recall", # rpd, rpiq, rsq
    "rv" = "recall_vec", # rpd_vec, rpdiq_vec
    "ra" = "roc_auc", # roc_aunnu, aunp
    "rav" = "roc_auc_vec",
    "rc" = "roc_curve",
    "s" = "sensitivity", # specfificity
    "sv" = "sensitivity_vec", # specificity_vec, spec_vec
    "ve" = "value_estimator",
    {
      message(paste(
        "first letter:",
        x[1],
        "unknown yardstick abbreviation"
      ))
      NA
    }
  )
}

##########  workflows ##########
#' Function to insert workflows shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_workflows <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^a|c|i|p|r|u|w{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized workflows starting chars:", workflows_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_workflows_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

### TODO:yarstick uses many abbreviation as function names. How to describe?
expand_workflows_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 4)
  x <- stringi::stri_c(x, collapse = "")
  switch(x,
    "af" = "add_formula",
    "am" = "add_model",
    "ar" = "add_recipe",
    "av" = "add_variables",
    "cw" = "control_workflow",
    "itw" = "is_trained_workflow",
    "pwf" = "pull_workflow_fit",
    "pwm" = "pull_workflow_mold",
    "pwpr" = "pull_workflow_prepped_recipe",
    "pwp" = "pull_workflow_preprocessor",
    "pws" = "pull_workflow_spec",
    "rf" = "remove_formula",
    "rv" = "remove_variables",
    "rr" = "remove_recipe",
    "rm" = "remove_model",
    "uf" = "update_formula",
    "ur" = "update_recipe",
    "um" = "update_model",
    "uv" = "update_variables",
    "w" = "workflow",
    "wv" = "workflow_variables",
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

##########  rsample ##########
#' Function to insert rsample shortcut combination into R script at cursor position
#'
#' @return Adjusted cursor position in R script
#' @export
construct_rsample <- function() {
  abb <- character(length = 1)
  abb <- svDialogs::dlg_input("Input function abbreviation",
    default = NULL)$res
  if (grepl(" ", abb, perl = TRUE)) {
    message("Invalid input: space detected in input")
    return(NULL)
  } else if (!grepl("^a|b|c|e|f|g|i|l|m|n|s|t|v{1}", abb, perl = TRUE)) {
    message(paste("Invalid input: unrecognized starting letter,
                    recognized rsample starting chars:", rsample_error))
    return(NULL)
  } else if (nchar(abb) > 5) {
    message("Input invalid: input too long, no such abbreviation length in package dictionary")
    return(NULL)
  }
  if (check_plot_context()) {
    warning("%>% operator mode spotted while plotting.\nsetting mode to +, use turbokit::toggle() or shortcut to switch back")
    invisible(toggle_pipe())
  }
  abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
  expression <- expand_rsample_abbreviation(abb)
  if (grepl("NA", expression, perl = TRUE)) {
    return(NULL)
  }
  rstudioapi::insertText(paste0(expression, "()"))
  reposition(1)
}

expand_rsample_abbreviation <- function(x) {
  stopifnot(length(x) > 0 & length(x) <= 5)
  out <- character(length(x))
  if (length(out) >= 2 && x[1] == "p") {
    return(expand_rsample_pretty(x))
  } else {
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
      "t" = "training", # testing
      "ari" = "add_sample_id",
      "a" = "analysis",
      "b" = "bootstraps",
      "c" = "complement", # caret2resample
      "fp" = "form_pred",
      "gr" = "gather.rset",
      "gvc" = "group_vfold_cv",
      "is" = "initial_split",
      "its" = "initial_time_split",
      "ib" = "int_bca",
      "ip" = "int_pct1",
      "it" = "int_t",
      "lc" = "loo_cv",
      "ms" = "make_strata", # make_splits
      "mr" = "manual_rset",
      "mc" = "mc_cv",
      "nc" = "nested_cv",
      "nr" = "new_rset",
      "p" = "permutations", # populate
      "ri" = "reg_intervals",
      "ro" = "rolling_origin",
      "r" = "rsample2caret",
      "rr" = "rset_reconstruct",
      "si" = "sliding_index",
      "sp" = "sliding_period",
      "sw" = "sliding_window",
      "vs" = "validation_split",
      "vc" = "vfold_cv",
      {
        message(paste(
          "first letter:",
          x[1],
          "unknown rsample abbreviation"
        ))
        NA
      }
    )
  }
}

expand_rsample_pretty <- function(x) {
  stopifnot(length(x) <= 5)
  out <- character(length(x))
  out[1] <- "pretty."
  out[2] <- sub(
    x = x[2],
    pattern = x[2],
    replacement = switch(x[2],
      "a" = "apparent",
      "b" = "bootstraps",
      "g" = "groups_vfold_cv",
      "l" = "loo_cv",
      "m" = "manual_rset",
      "n" = "nested_cv",
      "p" = "permutations",
      "s" = {
        if (x[3] == "i") {
          "sliding_index"
        } else if (x[3] == "p") {
          "sliding_period"
        } else if (x[3] == "w") {
          "sliding_window"
        }
      },
      "r" = "rolling_origin",
      "v" = "validation_split",
      {
        message(paste(
          "second letter:",
          x[1],
          "unknown rsample_pretty abbreviation"
        ))
        NA
      }
    ),
    fixed = TRUE
  )
  out <- paste0(out, collapse = "_")
  # remove first and last _
  out <- stringr::str_remove(string = out, pattern = "_")
  out <- stringr::str_remove(string = out, pattern = "_{1,2}$")
}
