test_that("potions objects are created with correct slots", {
  x <- create_potions()
  expect_equal(names(x), c("mapping", "slots", "packages"))
  expect_equal(unlist(lengths(x)), c(mapping = 2, slots = 0, packages = 0))
  expect_true(inherits(x, "potions"))
})