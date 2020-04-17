context("Players")
library(squashinformr)

test_that("test get_players for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_players(top = 1, rank = 1, category = "mens"))
  expect_error(get_players(top = "", category = "mens"))
  expect_error(get_players(rank = "", category = "mens"))
  expect_error(get_players(top = "", category = "womens"))
  expect_error(get_players(rank = "", category = "womens"))
  expect_error(get_players(top = "", category = "both"))
  expect_error(get_players(rank = "", category = "both"))
  expect_error(get_players(rank = "Mohamed Elshorbagy", category = "1"))
  expect_error(get_players(top = "", rank = "", category = "mens"))
  expect_error(get_players(top = NULL, rank = NULL, category = "mens"))

})

test_that("test get_players for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_players(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_players(top = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_players(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_players(top = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_players(rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_players(top = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
