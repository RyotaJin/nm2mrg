test_that("correct", {
  expect_equal(replace_pow_from_string("3^2"), "pow(3, 2)")
})

test_that("correct", {
  expect_equal(replace_pow_from_string("3**2"), "pow(3, 2)")
})

test_that("correct", {
  expect_equal(replace_pow_from_string("exp(3)^2"), "pow(exp(3), 2)")
})

test_that("correct", {
  expect_equal(replace_pow_from_string("LOG(3)^2"), "pow(LOG(3), 2)")
})

test_that("correct", {
  expect_equal(replace_pow_from_string("3 * (2**2 + 2)**3"), "3 * pow((pow(2, 2) + 2), 3)")
})
