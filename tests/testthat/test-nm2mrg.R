test_that("correct", {
  result <- nm2mrg(mod_name = "sample1", dir = "../../inst/extdata/")

  expected <- paste(readLines(testthat::test_path("../../inst/extdata/sample1.cpp")), collapse = "\n")
  expected <- paste0(expected, "\n\n")

  expect_equal(result, expected)
})
