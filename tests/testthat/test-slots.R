test_that("`brew` sets up an empty `potions` object when called with no args", {
  brew()
  x <- explore_potions()
  expect_true(inherits(x, "potions"))
  expect_equals(length(x), 3)
  expect_equals(length(unlist(x)), 1)
  options(list("potions-pkg" = NULL)) # set to empty
})

test_that("`brew` stores data in a randomly-named slot when no args given", {
  options_list <- list(
    data = list(x = 1, y = 2),
    metadata = list(a = 10, b = 12))
  brew(options_list)
  x <- explore_potions()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1, 2, 10, 12))
  options(list("potions-pkg" = NULL))
})

test_that("sequential, unlabelled called to `brew` get added to the same slot",{
  brew(list(x = 1))
  brew(list(y = 2))
  x <- explore_potions()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1, 2))
  expect_equal(names(x$slots[[1]]), c("x", "y"))
  expect_equal(as.integer(lengths(x)), c(2, 1, 0))
  options(list("potions-pkg" = NULL))
})


test_that("`brew` stores data in .slot when given", {
  options_list <- list(
    data = list(x = 1, a = 2),
    metadata = list(x = 10, b = 12))
  brew(options_list, .slot = "a_test")
  x <- explore_potions()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1, 2, 10, 12))
  expect_equal(names(x$slots[[1]]), c("data", "metadata"))
  options(list("potions-pkg" = NULL))
})

test_that("`brew` updates data in .slot when given", {
  options_1 <- list(
    data = list(x = 1, a = 2),
    metadata = list(x = 10, b = 12))
  options_2 <- list(
    data = 1,
    something = 2)
  
  brew(options_1, .slot = "test")
  brew(options_2, .slot = "test")
  x <- explore_potions()
  
  expect_equal(names(x$slots)[1], "test")
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(length(x$slots[[1]]), 3)
  expect_equal(as.integer(lengths(x)), c(2, 1, 1))
  expect_equal(names(result), c("metadata", "data", "something"))
  options(list("potions-pkg" = NULL))
})

