test_that("`brew` sets up an empty `potions` object when called with no args", {
  brew()
  x <- pour_all()
  expect_true(inherits(x, "potions"))
  expect_equal(length(x), 3)
  expect_equal(length(unlist(x)), 1)
  options(list("potions-pkg" = NULL)) # set to empty
})

test_that("`brew` accepts data passed within functions", {
  testfun <- function(){list(x = 1, y = 2)}
  brew(testfun())
  expect_equal(pour(), list(x = 1, y = 2))
  rm(testfun)
  options(list("potions-pkg" = NULL)) # set to empty
})

test_that("`brew` fails when objects are passed that are not lists", {
  expect_error(brew(integer(1, 2, 3)))
})

test_that("`brew` fails when user passes `==` instead of `=`", {
  expect_error(brew(year == 10))
})

test_that("`brew()` fails when some levels are unnamed", {
  options_list <- list(
    user = list(uuid = "owner-uuid"),
    params = list(
      function_name = "ensemble",
      datasets = list(
        list(mimetype = "image/geotiff", # unnamed step
             url = "a/valid/path_1"),
        list(mimetype = "image/geotiff",
             url = "a/valid/path_2")
      )
    )
  )
  expect_error(brew(options_list))
  rm(options_list)
})


test_that("`brew` stores data in a randomly-named slot when .slot not given", {
  options_list <- list(
    data = list(x = 1, y = 2),
    metadata = list(a = 10, b = 12))
  brew(options_list)
  x <- pour_all()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1, 2, 10, 12))
  options(list("potions-pkg" = NULL))
})

test_that("sequential, unlabelled calls to `brew` get added to the same slot",{
  brew(x = 1)
  brew(y = 2)
  x <- pour_all()
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
  x <- pour_all()
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
  x <- pour_all()
  
  expect_equal(names(x$slots)[1], "test")
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(length(x$slots[[1]]), 3)
  expect_equal(as.integer(lengths(x$slots[[1]])), c(1, 2, 1))
  expect_equal(names(x$slots[[1]]), c("data", "metadata", "something"))
  options(list("potions-pkg" = NULL))
})

test_that("`drain` removes data when no args given", {
  brew(list(x = 1))
  x <- pour_all()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1))
  drain()
  x <- pour_all()
  expect_equal(length(x$slots[[1]]), 0)
  options(list("potions-pkg" = NULL))
})

# test_that("drain works when `.slot` is set", {
#   brew("potions_test_1", list(x = 1))
#   brew("potions_test_2", list(x = 1))
#   drain("potions_test_1")
#   # test using {base}
#   expect_null(getOption("potions_test_1"))
#   expect_true(inherits(getOption("potions_test_2"), "list"))
#   # test using {potions}
#   expect_null(pour(slot_name = "potions_test_1"))
#   expect_true(inherits(pour(), "list"))
#   # clean up
#   drain("potions_test_2")
#   expect_null(pour(slot_name = "potions_test_2"))
# })

test_that("`brew()` detects current slot during successive calls", {
  brew(x = 10)
  result1 <- pour_all()
  brew(x = 10)
  result2 <- pour_all()
  expect_equal(result1, result2)
  rm(result1, result2)
  options(list("potions-pkg" = NULL))
})

test_that("`brew()` doesn't call names from the Global environment", {
  x <- "something"
  brew(x = 10)
  result <- pour()
  expect_equal(names(result), "x")
  rm(result, x)
  options(list("potions-pkg" = NULL))
})

test_that("`brew()` works when `method = leaves`", {
  brew(list(data1 = list(x = 1, y = 2), 
            data2 = list(a = 5, b = 10)))
  brew(x = 20, b = 50, method = "leaves")
  expect_equal(pour(), 
               list(data1 = list(x = 20, y = 2), 
                    data2 = list(a = 5, b = 50)))
  options(list("potions-pkg" = NULL))
})

## NOTE:
## methods "merge" and "overwrite" are not tested yet!