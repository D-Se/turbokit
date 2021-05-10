test_directory <- system.file("test_data", package = "turbokit")
test_file <- file.path(test_directory, "test.snippets")
test_file2 <- file.path(test_directory, "test2.snippets")

test_that("snippets can be added", {
  snippets <- read_snippet(path = test_file)
  expect_equal(length(snippets), 3)
  expect_equal(names(snippets), c("sc", "sg", "sm"))
})

test_that("snippet called s is detected", {
  snippets <- read_snippet(path = test_file2)
  expect_equal(names(snippets), c("sc", "sg", "sm", "s"))
})
