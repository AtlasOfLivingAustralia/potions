test_that("pour returns a list when length > 1", {
  brew(x = 1, y = 2)
  result <- pour()
  expect_true(inherits(result, "list"))
  expect_equal(length(result), 2)
  rm(result)
  options(list("potions-pkg" = NULL))
})

test_that("pour returns a vector when length == 1", {
  brew(x = 1L, y = 2L)
  result <- pour("x")
  expect_true(inherits(result, "integer"))
  expect_equal(length(result), 1)
  rm(result)
  options(list("potions-pkg" = NULL))
})

test_that("pour works with non-repeated names", {
  options_list <- list(
    data = list(x = 1, y = 2),
    metadata = list(a = 10, b = 12))
  brew(options_list)
  # pour_all()
  result <- pour("data", "y")
  expect_equal(length(result), 1)
  expect_equal(result, 2)
  # clean up
  rm(result, options_list)
  options(list("potions-pkg" = NULL))
})

test_that("pour works with repeated names", {
  brew(list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12)))
  # quoted
  result <- pour("data", "a")
  expect_equal(length(result), 1)
  expect_equal(result, 2)
  # clean up
  rm(result)
  options(list("potions-pkg" = NULL))
})

test_that("pour returns whole object if no args are given", {
  options_list <- list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12))
  brew(options_list)
  result <- pour()
  expect_true(inherits(result, "list"))
  expect_equal(length(result), 2)
  options(list("potions-pkg" = NULL))
})

test_that("pour returns null with incorrect slot names", {
  options_list <- list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12))
  brew(options_list)
  result <- pour("data", "y")
  expect_null(result)
  # clean up
  rm(result)
  options(list("potions-pkg" = NULL))
})

test_that("pour errors when NSE is used", {
  brew(x = 1)
  expect_error(pour(x))
  options(list("potions-pkg" = NULL))
})