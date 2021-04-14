context("Players")

test_that("test get_players for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Both top and rank provided
  expect_error(squashinformr::get_players(top = 1, rank = 1, category = "mens"))

  ## top and rank as empty strings
  expect_error(squashinformr::get_players(top = "", category = "mens"))
  expect_error(squashinformr::get_players(rank = "", category = "mens"))
  expect_error(squashinformr::get_players(top = "", category = "womens"))
  expect_error(squashinformr::get_players(rank = "", category = "womens"))
  expect_error(squashinformr::get_players(top = "", category = "both"))
  expect_error(squashinformr::get_players(rank = "", category = "both"))

  ## Rank is character, category is not valid
  expect_error(squashinformr::get_players(rank = "Mohamed Elshorbagy", category = "1"))

  ## top and rank are both provided and are empty strings or NULL
  expect_error(squashinformr::get_players(top = "", rank = "", category = "mens"))
  expect_error(squashinformr::get_players(top = NULL, rank = NULL, category = "mens"))

  ## category is not valid
  expect_error(squashinformr::get_players(top = 1, category = "man"))

})

test_that("test get_players for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- squashinformr::get_players(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- squashinformr::get_players(top = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- squashinformr::get_players(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- squashinformr::get_players(top = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- squashinformr::get_players(rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- squashinformr::get_players(top = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
