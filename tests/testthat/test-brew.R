test_that("`brew` fails when slot_name is missing", {
  options_list <- list(
    data = list(x = 1, y = 2),
    metadata = list(a = 10, b = 12))
  expect_error(brew(x = options_list))
})

test_that("`brew` fails when x is missing", {
  expect_error(brew(slot_name = "something"))
  expect_null(getOption("something"))
})

test_that("`brew` stores data in correct slot", {
  options_list <- list(
    data = list(x = 1, a = 2),
    metadata = list(x = 10, b = 12))
  brew("potions_test", options_list)
  result <- getOption("potions_test")
  expect_equal(length(result), 2)
  expect_equal(as.integer(lengths(result)), c(2, 2))
  expect_equal(names(result), c("data", "metadata"))
  drain()
})

test_that("`brew` updates data correctly", {
  options_1 <- list(
    data = list(x = 1, a = 2),
    metadata = list(x = 10, b = 12))
  options_2 <- list(
    data = 1,
    something = 2)
  
  brew("potions_test", options_1)
  brew("potions_test", options_2)
  
  result <- getOption("potions_test")
  expect_equal(length(result), 3)
  expect_equal(as.integer(lengths(result)), c(2, 1, 1))
  expect_equal(names(result), c("metadata", "data", "something"))
  drain()
})