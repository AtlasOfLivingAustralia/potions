
testthat("`brew` works for packages", {
  # load a testing package
  pkg_path <- test_path("potionstest")
  pkgload::load_all(pkg_path)
  
  # check object exists
  # NOTE: This is only true because brew(.pkg = "package_name") is called in
  # `testpotions/onload.R`, as instructed in `potions/README.md`.
  # Ergo this test is a validation of our recommended workflow
  x <- explore_potions()
  expect_false(is.null(x))
  expect_false(is.null(x$mapping$packages))
  expect_equal(as.character(unlist(x$mapping)), "potionstest")
  
  # run a function that requires `brew` to update within a package
  brewtest_nopkg(list(x = 1)) # internally calls `brew()`
  x <- explore_potions()
  expect_equal(as.character(unlist(x$mapping)), "potionstest")
  expect_equal(x$packages$potionstest, list(x = 1))
  
  # try `pour()`
  # access pour() within another package
  expect_equal(pourtest_nopkg(), list(x = 1))
  # try pour() when `brew` has only been called within a package
  expect_message(pour()) # returns NULL and message saying no data stored
  
  # add an extra call to brew() within a package
  brewtest_nopkg(list(y = 2))
  x <- explore_potions()
  
  # clean up
  pkgload::unload("potionstest") # package
  options(list("potions-pkg" = NULL)) # options
})
