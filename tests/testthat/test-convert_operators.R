test_that("correct", {
  expect_equal(convert_operators("T.EQ.3"), "T==3")
})

test_that("correct", {
  expect_equal(convert_operators("T.ne.3"), "T!=3")
})
