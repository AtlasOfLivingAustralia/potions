test_that("`brew` works for packages", {
  # load a testing package
  pkg_path <- testthat::test_path("potionstest")
  pkgload::load_all(pkg_path, quiet = TRUE)
  
  # check object exists
  # NOTE: This is only true because brew(.pkg = "package_name") is called in
  # `testpotions/onload.R`, as instructed in `potions/README.md`.
  # Ergo this test is a validation of our recommended workflow
  x <- pour_all()
  expect_false(is.null(x))
  expect_false(is.null(x$mapping$packages))
  expect_equal(as.character(unlist(x$mapping)), "potionstest")
  rm(x)
  
  # run a function that requires `brew` to update within a package
  brewtest_nopkg(list(x = 1)) # internally calls `brew()`
  x <- pour_all()
  expect_equal(as.character(unlist(x$mapping)), "potionstest")
  expect_equal(x$packages$potionstest, list(x = 1))
  rm(x)
  
  # try `pour()`
  # access pour() within another package
  expect_equal(pourtest_nopkg(), list(x = 1))
  # try pour() when `brew` has only been called within a package
  expect_message(pour()) # returns NULL and message saying no data stored
  
  # add an extra call to brew() within a package
  brewtest_nopkg(list(y = 2))
  x <- pour_all()
  expect_equal(x$packages$potionstest, list(x = 1, y = 2))
  rm(x)
  
  # test drain()
  draintest_nopkg()
  x <- pour_all()
  expect_equal(names(x$packages), "potionstest")
  expect_equal(x$packages$potionstest, list())
  
  # clean up
  pkgload::unload("potionstest") # package
  options(list("potions-pkg" = NULL)) # options
})

test_that("`brew() with no args defaults to brew_interactive, even when a pkg is loaded", {
  # load a testing package
  pkg_path <- testthat::test_path("potionstest")
  pkgload::load_all(pkg_path, quiet = TRUE)
  
  # use brew to store data, BUT NOT from within the loaded package
  ## expected behaviour; added to .slot with random name
  brew(list(x = 1))
  x <- pour_all()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1))
  
  # clean up
  pkgload::unload("potionstest") # package
  options(list("potions-pkg" = NULL))
})

test_that("`drain()` removes data from .slot when no args given, even if a .pkg has been loaded", {
  # load a testing package
  pkg_path <- testthat::test_path("potionstest")
  pkgload::load_all(pkg_path, quiet = TRUE)
  
  # add package data
  brewtest_nopkg(list(x = 1, y = 2))

  # use brew to store data, BUT NOT from within the loaded package
  ## expected behaviour; added to .slot with random name
  brew(list(x = 1))
  x <- pour_all()
  expect_equal(x$mapping$current_slot, names(x$slots)[1])
  expect_equal(as.integer(unlist(x$slots)), c(1))
  expect_equal(as.integer(unlist(x$packages)), c(1, 2))
  
  # clean up
  pkgload::unload("potionstest") # package
  options(list("potions-pkg" = NULL))
})