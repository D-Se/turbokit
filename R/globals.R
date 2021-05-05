####### complex ######
turbokit_packages <- c(
  "ggplot", "stringr", "forcats", "clock",
  "readr", "recipes", "parsnip", "tune",
  "dials", "yarstick", "workflow",
  "shiny", "rsample", "ggmisc", "cowplot",
  "ggsci", "ggthemes", "ggforce", "ggridges",
  "ggraph", "usethis", "testthat", "scales"
)
### TODO: make comprehensive list of combinations and "guessing" positions.
### NOTE changes in .patterns MUST correspond to changes in .replacements!
patterns <- c(
  "read",
  "mydata",
  "ggplot\\(aes\\(\\)\\)",
  "mutate\\(\\)",
  "mutate\\(across\\(\\)\\)",
  "mutate\\(case_when\\(\\)\\)",
  "filter\\(\\)",
  "summarise\\(\\)",
  "summarise\\(across\\(\\)\\)",
  "summarise\\(across\\(everything\\(\\)\\)\\)",
  "select\\(\\)",
  "select\\(starts_with\\(\\)\\)",
  "select\\(ends_with\\(\\)\\)",
  "select\\(contains\\(\\)\\)",
  "select\\(matches\\(\\)\\)",
  "select\\(num_range\\(\\)\\)",
  "select\\(last_col\\(\\)\\)",
  "select\\(where\\(\\)\\)",
  "group_by\\(\\)",
  "case_when\\(\\)",
  "across\\(\\)",
  "theme\\(\\)",
  "theme\\(element_text\\(\\)\\)"
)

# if "fun" is used, the default fun snippet is inserted when TAB is hit.
replacements <- c(
  "read(${0:package})",
  "${0:mydata}",
  "ggplot(aes(${0:params}))",
  "mutate(${0:var})",
  "mutate(across(${0:var}))",
  "mutate(${0:var} = case_when()",
  "filter(${0:var})",
  "summarise(${0:var})",
  "summarise(across(${0:var}))",
  "summarise(across(everything(), ${0:func}))",
  "select(${0:var})",
  "select(starts_with(\"${0:charmatch}\"))",
  "select(ends_with(\"${0:charmatch}\"))",
  "select(contains(\"${0:charmatch}\"))",
  "select(matches(\"${0:charmatch}\"))",
  "select(num_ranges(\"${0:charmatch}\"))",
  "select(last_col(\"${0:charmatch}\"))",
  "select(where(\"${0:function}\"))",
  "group_by(${0:var})",
  "case_when(${0:var})", # mechanism to construct complex case_when?

  "across(${0:var})",

  ### TODO: repair for-loop in .construct_snippet to give 1, 2 position
  "theme(${0:param} = ${0:param})",
  "theme(${0:param} = element_text(${0:param}))"
)
NAMED_PATTERNS <- setNames(object = replacements, nm = patterns)

##### error message strings ######
complex_error <- c("1: ggplot, 2: stringr, 3: forcats, 4: clock, 5: readr.")

ggplot_error <- c("g, s, c, f, e, p, t")
stringr_error <- c("s, w")
clock_error <- c("a, d, g, i, s, w, z")
forcats_error <- c("d, a, l")
readr_error <- c("r, w, t, c, d, f, m, p, s")

recipes_error <- c("r, b, j, p, a, s, c, h, u")
parsnip_error <- c("d, b, c, e, g")
tune_error <- c("c, e, f, l, m, o, p, r, s, t")
dials_error <- c("a, b, c, d, e, f, g, h, i, k, l, m, n, o, p, r, s, t, u, v , w")
yardstick_error <- c("a, b , c, d, f, g, h, i, j, m, n, p, r, s, t, v")
workflows_error <- c("a, c, i, p, r, u, w")
rsample_error <- c("a, b , c, e, f, g, i, l, m, n, s, t, v")

shiny_error <- c("a, b, c, d, e, f, h, i, k, l, m, n, o , p, q, r, s, t, u, v")

usethis_error <- c("b, c, e, g, i, l, p, r, t, u, w")
testthat_error <- c("a, c, d, e, f, g, h, i, l, m, n, p, q, r, s, t, u, v, w")

ggmisc_error <- c("a, c, g, p, s, e, t, u, f, r")
cowplot_error <- c("g, p, d, s, t")
ggsci_error <- c("p, r, s")
ggthemes_error <- c("s, t, w, h, c, e, p")
ggforce_error <- c("f, g, l, n, p, r, s, t, i")
ggridges_error <- c("c, g, p, s, t")
ggraph_error <- c("a, c, e, f, g, i, l, n, r, s, t, u")
scales_error <- c("a, b, c, d, e, f, g, h, i, l, m, n, o, p, r, s, t, u, v, w, y, z")

# avoid NOTE during roxygen check due to NSE
utils::globalVariables(c(
  "new",
  "input",
  "across",
  "case_when",
  "contains",
  "ends_with",
  "everything",
  "insert_ggplot",
  "filter",
  "full_join",
  "ggplot",
  "aes",
  "group_by",
  "inner_join",
  "left_join",
  "mutate",
  "nest",
  ".rs.readUiPref",
  "pivot_longer",
  "pivot_wider",
  "right_join",
  "select",
  "starts_with",
  "summarise",
  "unnest",
  "where",
  "line", # NSE
  "snippet" # NSE
))
