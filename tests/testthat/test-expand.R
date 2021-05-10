test_that("turbo expands yielts correct abbreviation", {
  expect_equal(expand_tidyverse("1gh"), "geom_histogram")
  expect_equal(expand_tidymodels("1sh"), "step_holiday")
  expect_equal(expand_shiny("1rv"), "reactiveValues")
  expect_equal(expand_visualisation("6ppj"), "position_points_jitter")
})
