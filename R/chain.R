#' Transformation step of dialog box input to be construct_* function-friendly
#'
#' @param input character string to transform to snippet syntax
#' @return character vector stripped of any numeric indicator
construct_chain <- function(input) {
  mode <- mode_toggle()$mode
  chain <- as.list(strsplit(x = input, "(?<=[>])", perl = TRUE)[[1]])
  chain <- lapply(chain, function(x) {
    unlist(strsplit(x = x, "(?=[!,>*~])", perl = TRUE))
  })
  if (chain[[1]][2] == "!") {
    chain <- unlist(chain)
    n <- sum(stringi::stri_count(str = chain, regex = "!"))
    return(rep("read", n))
  }
  chain <- lapply(chain, function(x) {
    unlist(
      lapply(x, FUN = function(y) {
        if (grepl(x = y, pattern = "^[[:digit:]]", perl = TRUE)) {
          if (mode == "tidyverse") {
            expression <- expand_tidyverse(y)
          } else if (mode == "tidymodels") {
            expression <- expand_tidymodels(y)
          } else if (mode == "shiny") {
            expression <- expand_shiny(y)
          }
          paste0(expression, "()")
        } else if (nchar(y) > 1) {
          expression <- switch(y,
            "sw" = "starts_with",
            "ew" = "ends_with",
            "rj" = "right_join",
            "lj" = "left_join",
            "cw" = "case_when",
            NA
          )
          paste0(expression, "()")
        } else {
          expression <- switch(y,
            ">" = y,
            "*" = y,
            "~" = y,
            "m" = "mutate",
            "f" = "filter",
            "s" = "select",
            "p" = "ggplot",
            "z" = "summarise",
            "a" = "across",
            "e" = "everything",
            "g" = "group_by",
            "," = y,
            NA
          )
          if (startsWith(expression, ",") | startsWith(expression, ">") |
            startsWith(expression, "*") | startsWith(expression, "~")) {
            y
          } else {
            paste0(expression, "()")
          }
        }
      })
    )
  })
  if (mode == "tidyverse") {
    chain <- append(chain, values = list(c("mydata", ">")), after = 0)
    chain[[2]] <- chain[[2]][-1]
  } else {
    chain[[1]] <- chain[[1]][-1]
  }
  chain <- lapply(chain, function(x) {
    if (any(grepl("[*]", x = x, perl = TRUE))) {
      x <- transform_complex_interaction(x)
    }
    sub(x = x, pattern = ">", replacement = "%>%", fixed = TRUE)
  })
  chain <- lapply(
    lapply(chain[!!lengths(chain)], toString),
    function(x) {
      sub(
        x = x, pattern = ",",
        replacement = "",
        fixed = TRUE
      )
    }
  )
  chain <- lapply(chain, function(x) {
    if (any(grepl(
      x = x,
      pattern = "ggplot|geom|facet|position|scale|coord|element|theme",
      perl = TRUE
    ))) {
      x <- sub(
        x = x, pattern = "%>%",
        replacement = "+",
        fixed = TRUE
      )
      x <- sub(
        x = x, pattern = "ggplot\\(\\)",
        replacement = "ggplot(aes())",
        perl = TRUE
      )
    } else {
      x
    }
  })
  # remove any operator left at the end
  chain[[length(chain)]] <- sub(chain[[length(chain)]],
    pattern = "\\+|\\%>\\%",
    replacement = "",
    perl = TRUE
  )
  styler::style_text(sapply(chain[!!lengths(chain)], toString))
}
