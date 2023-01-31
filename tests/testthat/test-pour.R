test_that("pour works with non-repeated names", {
  options_list <- list(
    data = list(x = 1, y = 2),
    metadata = list(a = 10, b = 12))
  brew("potions_test", options_list)
  result <- pour("data", "y")
  expect_equal(length(result), 1)
  expect_equal(result, 2)
  drain()
})

test_that("pour works with repeated names", {
  options_list <- list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12))
  brew("potions_test", options_list)
  result <- pour("data", "a")
  expect_equal(length(result), 1)
  expect_equal(result, 2)
  drain()
})

test_that("pour returns null with incorrect slot names", {
  options_list <- list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12))
  brew("potions_test", options_list)
  result <- pour("data", "y")
  expect_null(result)
  drain()
})