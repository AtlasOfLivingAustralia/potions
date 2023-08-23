test_that("read_config_file works for yml", {
  # create data list, add to file
  config_list <- list(username = "mjwestgate",
                      organisation = "Atlas of Living Australia",
                      "package-potionstest" = list("date-accessed" = "2023-04-20"))
  yaml::write_yaml(config_list, file = "config.yml")
  # run read_config_file, check
  x <- read_config("config.yml")
  expect_equal(x, config_list)
  # clean up
  unlink("config.yml")
  rm(x, config_list)
  options(list("potions-pkg" = NULL))
})

test_that("read_config_file works for json", {
  # create data list, add to file
  config_list <- list(username = "mjwestgate",
                      organisation = "Atlas of Living Australia",
                      "package-potionstest" = list("date-accessed" = "2023-04-20"))
  jsonlite::write_json(config_list, path = "config.json", auto_unbox = TRUE)
  x <- read_config("config.json")
  expect_equal(x, config_list)
  unlink("config.json")
  rm(x, config_list)
  options(list("potions-pkg" = NULL))
})