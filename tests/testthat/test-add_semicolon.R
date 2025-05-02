test_that("correct", {
  expect_equal(add_semicolon("if (T<=3) {"), "if (T<=3) {")
})

test_that("correct", {
  expect_equal(add_semicolon("CL = 1"), "CL = 1;")
})
