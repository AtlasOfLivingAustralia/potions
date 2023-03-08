test_that("potions objects are created with correct slots", {
  x <- create_potions()
  expect_equal(names(x), c("mapping", "slots", "packages"))
  expect_equal(lengths(x), c(2, 0, 0))
  expect_equal(inherits(x), "potions")
})