# roxygen2 remotes pkgbuild pkgload rcmdcheck revdepcheck sessioninfo

##########  usethis ##########
#' Function to translate usethis package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_usethis <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (!grepl("^b|c|e|g|i|l|p|r|t|u|w{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized usethis starting chars:", .usethis_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (!pipe_toggle()$pipe == "%>%") {
        warning("+ operator mode spotted while making usethis call.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle_pipe())
    }
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
    expression <- .expand_usethis_abbreviation(abb)
    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to transform usethis abbreviation into character string
#'
#' @param x string starting with r.
#' @return Adjusted cursor position in R script
.expand_usethis_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    if (length(x) == 3 & grepl(x = x, pattern = "^i{1}")) {
        return(ifelse(x[2] == "c", "issue_close_community", "issue_reprex_needed"))
    }
    switch(
        x[1],
        "b" = .expand_usethis_browse(x),
        "c" = .expand_usethis_create(x),
        "e" = .expand_usethis_edit(x),
        "g" = .expand_usethis_git(x),
        "l" = "local_project",
        "p" = .expand_usethis_p(x),
        "r" = "rename_files",
        "t" = {
            dplyr::case_when(length(x) == 2 ~ "tidy_labels",
                             x[3] == "c" ~ "tidy_label_descriptions",
                             x[3] == "r" ~ "tidy_labels_rename")
        },
        "u" = .expand_usethis_use(x),
        "w" = {
            dplyr::case_when(x[2] == "p" ~ "write_project",
                             x[2] == "o" ~ "write_over",
                             x[2] == "u" ~ "write_union")
        },
        {
            message(paste("first letter:",
                          x[1],
                          "unknown usethis abbreviation"))
            NA
        }
    )
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_browse <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    out[1] <- "browse"
    if (length(out) == 2) {
        out[2] <- sub(x = x[2],
                      pattern = x[2],
                      replacement = switch(x[2],
                                           "c" = "cran", #circlei
                                           "g" = "github",
                                           "p" = "package",
                                           "t" = "travis",
                                           "n" = "nudge",
                                           "s" = "stack",
                                               {
                                                   message(paste(
                                                       "second letter:",
                                                       x[2],
                                                       "unknown usethis_browse abbreviation"))
                                                   NA
                                               }
                      ),
                      fixed = TRUE)
    } else if (length(out) == 3) {
        out[2] <- "github"
        out[3] <- sub(x = x[3],
                      pattern = x[3],
                      replacement = switch(x[2],
                                           "a" = "actions",
                                           "i" = "issues",
                                           "p" = "pulls",
                                           "t" = "token",
                                               {
                                                   message(paste(
                                                       "second letter:",
                                                       x[3],
                                                       "unknown usethis_browse abbreviation"))
                                                   NA
                                               }
                      ),
                      fixed = TRUE)
    }
    paste0(out, collapse = "_")
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_create <- function(x) {
    stopifnot(length(x) <= 4)
    out <- character(length(x))
    out[1] <- "create"
    if (length(out) == 2) {
        return("create_package")
    } else if (length(out) == 3) {
        x <- stringi::stri_c(x, collapse = "")
       switch(x,
              "cdu" = "create_download_url",
              "cfg" = "create_from_github",
              "cgt" = "create_github_token",
              "ctp" = "create_tidy_package",
        {
            message(paste(
                "second letter:",
                x[3],
                "unknown usethis_create abbreviation"))
            NA
        })
    }
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_edit <- function(x) {
    stopifnot(length(x) <= 4)
    out <- character(length(x))
    out[1] <- "create"
    if (length(out) == 2) {
        return(ifelse(x[2] == "f", "edit_file", "edit_template"))
    } else if (length(out) == 3) {
        x <- stringi::stri_c(x, collapse = "")
        switch(x,
               "egc" = "edit_git_config",
               "egi" = "edit_git_ignore",
               "erb" = "edit_r_buildignore",
               "ere" = "edit_r_environ",
               "erm" = "edit_r_makevars",
               "erp" = "edit_r_profile", #edit_rstudio_prefs
               "ers" = "edit_rstudio_snippets",
               {
                   message(paste(
                       "second letter:",
                       x[3],
                       "unknown usethis_edit abbreviation"))
                   NA
               })
    }
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_git <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    if (length(out) == 3) {
        return(ifelse(x[2] == "t", "gh_token_help", "git_branch_default"))
    }
    out[1] <- "git"
    out[2] <- sub(x = x[2],
                  pattern = x[2],
                  replacement = switch(x[2],
                                       "c" = "credentials",
                                       "p" = "protocol",
                                       "r" = "remotes",
                                       "s" = "sitrep",
                                       "v" = "vaccinate",
                                       "t" = "token", #edit_rstudio_prefs
                                       {
                                           message(paste(
                                               "second letter:",
                                               x[3],
                                               "unknown usethis_git abbreviation"))
                                           NA
                                       }))
    paste0(out, collapse = "_")
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_p <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    if (length(out) == 3) {
        return(ifelse(x[2] == "m", "pr_merge_main", "pr_pull_upstream"))
    }
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
           "pr" = "pr_fetch", #pr_finish forget
           "pi" = "pr_init",
           "pp" = "pr_pull",
           "ps" = "projsitrep", #pr_sync _proj_set
           "pg" = "proj_get",
           "pa" = "proj_activate",
           "pv" = "pr_view",
           {
               message(paste(
                   "second letter:",
                   x[3],
                   "unknown usethis_create abbreviation"))
               NA
           })
}

#' Function to translate usethis abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_usethis_use <- function(x) {
    stopifnot(length(x) <= 3)
    out <- character(length(x))
    if (length(out) == 3) {
        return(ifelse(x[2] == "m", "pr_merge_main", "pr_pull_upstream"))
    } else if (length(out) == 2) {
        out[1] <- "use"
        out[2] <- sub(x = x[2],
                      pattern = x[2],
                      replacement = switch(x[2],
                                           "a" = "addin", #article
                                           "b" = "badge",
                                           "c" = "coverage", #course circlei citation conflicted cpp11
                                           "d" = "data", #devtools
                                           "g" = "github", #git
                                           "j" = "jenkins",
                                           "l" = "lifecycle", #logo
                                           "m" = "make",
                                           "n" = "namespace",
                                           "p" = "use_package", #pipe pkgdown
                                           "ur" = "use_reprex",
                                           "ut" = "use_test",
                                           "uv" = "use_version", #vignette
                                           "uz" = "use_zip"
                                           ))
        return(paste0(out, collapse = "_"))
    } else if (length(out) == 3) {
        x <- stringi::stri_c(x, collapse = "")
        switch(x,
               "ual" = "use_apache_license",
               "uab" = "use_appveyor_badge",
               "ubb" = "use_binder_badge",
               "ubs" = "use_blank_slate",
               "ubi" = "use_blank_ignore",
               "ucl" = "use_ccby_license",
               "ucb" = "use_cran_badge",
               "ucc" = "use_cran_comments",
               "udt" = "use_data_table",
               "ugl" = "use_gpl13_license",
               "ulb" = "use_lifecycle_badge",
               "uml" = "use_mit_license",
               "upl" = "use_proprietary_license",
               "urt" = "use_rmarkdown_template",
               "urr" = "use_rmarkdown_rmd",
               "usc" = "use_spell_check",
               "ute" = "use_tidy_eval",
               {
                   message(paste(
                       "second letter:",
                       x[3],
                       "unknown usethis_create abbreviation"))
                   NA
               })
    } else if (length(out) == 4) return("use_code_of_conduct")
}

##########  testthat ##########

#' Function to translate testthat package shortcut combination into R script at
#' cursor position
#'
#' @return Adjusted cursor position in R script.
#' @export
construct_testthat <- function() {
    abb <- character(length = 1)
    abb <- svDialogs::dlg_input("Input function abbreviation",
                                default = NULL,
                                Sys.info()["user"])$res
    if (grepl(" ", abb, perl = TRUE)) {
        message("Invalid input: space detected in input")
        return(NULL)
    } else if (grepl("^b|j|o|x|y|z{1}", abb, perl = TRUE)) {
        message(paste("Invalid input: unrecognized starting letter,
                    recognized testthat starting chars:", .testthat_error))
        return(NULL)
    } else if(nchar(abb) > 5) {
        message("Input invalid: input too long, no such abbreviation length in package dictionary")
        return(NULL)
    }
    if (!pipe_toggle()$pipe == "%>%") {
        warning("+ operator mode spotted while making usethis call.\nsetting mode to %>%, use turbokit::toggle() or shortcut to switch back")
        invisible(toggle_pipe())
    }
    abb <- tolower(unlist(strsplit(abb, split = "", fixed = TRUE)))
    expression <- .expand_testthat_abbreviation(abb)
    if (grepl("NA", expression, perl = TRUE)) {
        return(NULL)
    }
    rstudioapi::insertText(paste0(expression, "()"))
    .reposition(1)
}

#' Function to transform usethis abbreviation into character string
#'
#' @param x string starting with r.
#' @return Adjusted cursor position in R script
.expand_testthat_abbreviation <- function(x) {
    stopifnot(length(x) > 0 & length(x) <= 3)
    out <- character(length(x))
    if (length(x) == 3 & grepl(x = x, pattern = "^i{1}")) {
        return(ifelse(x[2] == "c", "issue_close_community", "issue_reprex_needed"))
    }
    switch(
        x[1],
        "a" = {
            ifelse(length(out) == 3, "auto_test_package", "auto_test")
        },
        "c" = {dplyr::case_when(length(out) == 3 & x[2] == "o" ~ "capture_output_lines",
                                length(out) == 3 & x[2] == "s" ~ "context_start_file",
                                x[2] == "r" ~ "check_reported")},
        "d" = "describe",
        "e" = .expand_testthat_e(x),
        "f" = {
            ifelse(length(out) == 3, "find_test_scripts", "fail")
        },
        "g" = {ifelse(x[2] == "w", "gives_warning", "get_reported")},
        "h" = "has_names",
        "i" = {dplyr::case_when(x[2] == "p" ~ "is_parallel",
                                x[2] == "t" ~ "is_testing",
                                x[2] == "e" ~ "is.expectation")},
        "l" = .expand_testthat_l(x),
        "m" = {ifelse(length(out) == 1, "matches", "make_expectation")},
        "n" = {ifelse(length(out) == 1, "not", "new_expectation")},
        "p" = "prints_text",
        "q" = "quasi_label",
        "r" = "run_cpp_tests",
        "s" = .expand_testthat_s(x),
        "t" = .expand_testthat_t(x),
        "u" = "use_catch",
        "v" = "verify_output",
        "w" = {dplyr::case_when(length(out) == 1 ~ "watch",
                                x[2] == "m" ~ "with_mock",
                                x[2] == "m" ~ "with_reported")},
        {
            message("unknown testthat abbreviation")
            NA
        }
    )
}

#' Function to translate testthat abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_testthat_e <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    if (length(out) == 1) {
     return("expect") #expectations
    } else if (length(out) == 3) {
        x <- stringi::stri_c(x, collapse = "")
        switch(x,
               "ekh" = "expect_known_hash",
               "eko" = "expect_known_output",
               "ekv" = "expect_known_value",
               "elt" = "expect_less_than",
               "emt" = "expect_more_than",
               "eof" = "expect_output_file",
               "esc" = "expect_s3_class", #expect_s4_class
               "esf" = "expect_snapshot_file",
               "eso" = "expect_snapshot_output",
               "esv" = "expect_snapshot_value",
               "eme" = "expect_mapequal", #speial case
               "ese" = "expect_setequal", #expect_snapshot_error
               {
                   message("unknown testthat_e abbreviation")
                   NA
               })
    } else if (length(out) == 2) {
        if (x[2] == "p") {return("evaluate_promise")}
        out[1] <- "expect"
        out[2] <- sub(x = x[2],
                      pattern = x[2],
                      replacement = switch(x[2],
                                           "c" = "condition",
                                           "e" = "equal", #equivalent error
                                           "f" = "false", #failure
                                           "g" = "gt",
                                           "i" = "identitical", #invisible is
                                           "l" = "length", #lt lte
                                           "m" = "match", #message
                                           "n" = "named", #null
                                           "o" = "output",
                                           "r" = "reference",
                                           "s" = "silent", #snapshot
                                           "t" = "type", #true
                                           "v" = "visible", #vector
                                           "w" = "warning",
                                           ))
    }
}

#' Function to translate testthat abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_testthat_l <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
        x <- stringi::stri_c(x, collapse = "")
        switch(x,
               "le" = "local_edition",
               "lro" = "local_reproducible_output",
               "ls" = "local_snapshotter",
               "ltc" = "local_text_output",
               "ltd" = "local_text_directory",
               {
                   message("unknown testthat_l abbreviation")
                   NA
               })
}

#' Function to translate testthat abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_testthat_s <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    if (length(out) == 1) { return("succeed")} #skip
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
           "sf" = "show_failure", #source_file
           "sm" = "shows_message",
           "si" = "skip_if",
           "sin" = "skip_if_not",
           "sini" = "skip_if_not_installed",
           "sio" = "skip_if_offline",
           "sit" = "skip_if_translated",
           "soa" = "skip_on_appveyor",
           "sob" = "skip_on_bioc",
           "soc" = "skip_on_cran", #covr ci
           "soo" = "skip_on_os",
           "sot" = "skip_on_travis",
           "sa" = "snapshot_accept",
           "sr" = "snashot_review",
           "sd" = "source_directory",
           "sth" = "source_test_helpers",
           "sts" = "source_test_setup",
           "stt" = "source_test_teardown",
           {
               message("unknown testthat_s abbreviation")
               NA
           })
}

#' Function to translate testthat abbreviation starting with a into formatted
#' character string
#'
#' @param x character vector, maximum length 2.
#' @return full length character string of input.
.expand_testthat_t <- function(x) {
    stopifnot(length(x) <= 2)
    out <- character(length(x))
    if (length(out) == 3) { return("takes_less_than")}
    x <- stringi::stri_c(x, collapse = "")
    switch(x,
           "te" = "test_examples", #env example
           "tc" = "test_check",
           "ta" = "try_again",
           "td" = "test_dir", #testthat
           "tf" = "test_file", #testthat
           "tl" = "test_local",  #testthat
           "tp" = "test_package",
           {
               message("unknown testthat_t eabbreviation")
               NA
           })
}

#' Function to construct abbreviation into character string
#'
#' @param x character string starting with g.
#' @return Adjusted cursor position in R script
.expand_dev_default <- function(x){
    x <- stringi::stri_c(x, collapse = "")
    if (nchar(x) == 1) {
        switch(x,
               "b" = "build", #bash
               "c" = "check", #create
               "d" = "document",
               "f" = "find_rtools",
               "i" = "install",
               "r" = "reload", #release
               "t" = "test",
               "w" = "wd",
               {
                   message("unknown devtools abbreviation")
                   NA
               })
    } else if (nchar(x) == 2) {
        switch(x,
               "bm" = "build_manual",
               "br" = "build_readme", #build_rmd
               "bs" = "build_sit",
               "bv" = "build_vignettes",
               "cb" = "check_built",
               "cm" = "check_man",
               "cr" = "check_rhub",
               "cd" = "clean_dll",
               "cv" = "clean_vignettes",
               "dm" = "dev_mode",
               "dp" = "dev_packages",
               "ds" = "dev_sitrep",
               "fr" = "find_rtools",
               "gp" = "github_pull",
               "gr" = "github_release",
               "hd" = "has_devel",
               "ht" = "has_tests",
               "ib" = "install_bioc",
               "ic" = "install_cran",
               "id" = "install_deps",
               "ig" = "install_github",
               "il"= "install_local",
               "is"= "install_svn",
               "iu"= "install_url",
               "iv"= "install_version",
               "ip" = "is.package",
               "la" = "load_all",
               "lp" = "loaded_packages",
               "pf" = "package_file",
               "pi" = "package_info",
               "pd" = "parse_deps",
               "rm" = "revdep_maintainers",
               "re" = "run_examples",
               "si" = "session_info",
               "sn" = "show_news",
               "sg" = "source_gist",
               "su" = "source_url",
               "sc" = "spell_check", #submit_cran
               "tc" = "test_coverage",
               "tf" = "test_file",
               "os" = "os_name",
               {
                   message("unknown devtools abbreviation")
                   NA
               })
    } else if (nchar(x) == 3) {
        switch(x,
               "cdv" = "check_dev_version",
               "cwd" = "check_win_devel",
               "cwo" = "check_win_oldrelease",
               "cwr" = "check_win_release",
               "dpd" = "dev_package_deps",
               "idd" = "install_dev_deps",
               "tav" = "test_active_file",
               "tcf" = "test_coverage_file",
               {
                   message("unknown devtools abbreviation")
                   NA
               })
    }
}

