test_that("drain works without arguments", {
  brew("potions_test", list(x = 1))
  drain()
  expect_null(getOption("potions_test"))
})

test_that("drain works when `slot_name` is set", {
  brew("potions_test_1", list(x = 1))
  brew("potions_test_2", list(x = 1))
  drain("potions_test_1")
  # test using {base}
  expect_null(getOption("potions_test_1"))
  expect_true(inherits(getOption("potions_test_2"), "list"))
  # test using {potions}
  expect_null(pour(slot_name = "potions_test_1"))
  expect_true(inherits(pour(), "list"))
  # clean up
  drain("potions_test_2")
  expect_null(pour(slot_name = "potions_test_2"))
})