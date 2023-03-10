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

test_that("pour returns whole object if no args are given", {
  options_list <- list(
    data = list(x = 1, a = 2),
    data = list(x = 10, b = 12))
  brew("potions_test", options_list)
  result <- pour()
  expect_true(inherits(result, "list"))
  expect_equal(length(result), 2)
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

test_that("pour works when some levels are unnamed", {
  options_list <- list(
    user = list(uuid = "owner-uuid"),
    params = list(
      function_name = "ensemble",
      datasets = list(
        list(mimetype = "image/geotiff",
             url = "a/valid/path_1"),
        list(mimetype = "image/geotiff",
             url = "a/valid/path_2")
      )
    )
  )
  brew("potions_test", options_list)
  result <- pour("params", "datasets", "url")
  expect_equal(result, c("a/valid/path_1", "a/valid/path_2"))
  drain()
})
