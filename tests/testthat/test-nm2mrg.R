test_that("correct", {
  result <- nm2mrg(mod_name = "sample1", dir = "../../inst/extdata/")

  expected <- paste(readLines(testthat::test_path("../../inst/extdata/sample1.cpp")), collapse = "\n")
  expected <- paste0(expected, "\n")

  expect_equal(result, expected)
})

test_that("correct", {
  expected <- paste(readLines(testthat::test_path("../../inst/extdata/sample2.cpp")), collapse = "\n")
  expected <- paste0(expected, "\n")

  expect_warning(result <- nm2mrg(mod_name = "sample2", dir = "../../inst/extdata/"),
                 "Some covariates were detected. The initial value is set to 1 by default. Please update it as needed.")
  expect_equal(result, expected)
})
