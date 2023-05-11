test_that("`check_missing_names()` fails at correct times", {
  # example with no missing entries (should not error)
  x <- list(data = list(x = 1))
  check_missing_names(x)
  expect_equal(names(x), "data")
  expect_equal(names(x[[1]]), "x")
  
  # named first element, but not later
  x <- list(a = list(list(2)))
  expect_error(check_missing_names(x))
  
  # completely unnamed
  x <- list(list(list(2)))
  expect_error(check_missing_names(x))
  
  # unnamed followed by named
  x <- list(list(z = 1))
  expect_error(check_missing_names(x))
  rm(x)
  
  # # named using numbers - weirdly this works as they get quoted with ``
  # x <- list("1" = list("2" = list(2)))
  # expect_error(check_missing_names(x))
})