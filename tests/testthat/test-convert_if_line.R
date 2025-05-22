test_that("correct", {
  expect_equal(convert_if_line("IF (T.GE.3) THEN"), "if (T>=3) {")
})

test_that("correct", {
  expect_equal(convert_if_line("IF (T.GE.3) CL = 0"), "if (T>=3) CL = 0")
})

test_that("correct", {
  expect_equal(convert_if_line("ELSE IF (t<=4) then"), "} else if (t<=4) {")
})

test_that("correct", {
  expect_equal(convert_if_line("ELSE"), "} else {")
})

test_that("correct", {
  expect_equal(convert_if_line("ENDIF"), "}")
})

test_that("correct", {
  expect_equal(convert_if_line("CL = 1"), "CL = 1")
})
