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

test_that("sample3 keeps only code before nm2mrg_drop", {
  result <- nm2mrg(mod_name = "sample3", dir = "../../inst/extdata/")

  expected <- paste(readLines(testthat::test_path("../../inst/extdata/sample3.cpp")), collapse = "\n")
  expected <- paste0(expected, "\n")

  expect_equal(result, expected)
  expect_false(grepl("BLQ", result, fixed = TRUE))
  expect_false(grepl("LLOQ", result, fixed = TRUE))
  expect_false(grepl("F_FLAG", result, fixed = TRUE))
})

test_that("sample4 drops $ERROR entirely when marker is missing", {
  expected <- paste(readLines(testthat::test_path("../../inst/extdata/sample4.cpp")), collapse = "\n")
  expected <- paste0(expected, "\n")

  expect_warning(
    result <- nm2mrg(mod_name = "sample4", dir = "../../inst/extdata/"),
    "$ERROR was dropped because no 'nm2mrg_drop' marker was found.",
    fixed = TRUE
  )

  expect_equal(result, expected)
  expect_false(grepl("$ERROR", result, fixed = TRUE))
})
