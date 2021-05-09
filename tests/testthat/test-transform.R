test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("transform strings", {
    # removes *
    expect_equal(transform_complex_interaction(
        c("a", "*", "e", "*", "b")), c("bea", ">"))
    # insert a string at index, needed for transform_complex
    expect_equal(
        reposition_str("test this", "insert", 2),
        c("tinsertest this"))
})
