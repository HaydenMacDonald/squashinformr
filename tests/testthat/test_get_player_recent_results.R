context("Player Recent Results")
library(squashinformr)

test_that("test get_player_recent_results for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_player_recent_results(player = 1, rank = 1, category = "mens"))
  expect_error(get_player_recent_results(player = "", category = "mens"))
  expect_error(get_player_recent_results(rank = "", category = "mens"))
  expect_error(get_player_recent_results(player = "", category = "womens"))
  expect_error(get_player_recent_results(rank = "", category = "womens"))
  expect_error(get_player_recent_results(player = "", category = "both"))
  expect_error(get_player_recent_results(rank = "", category = "both"))
  expect_error(get_player_recent_results(rank = "Mohamed Elshorbagy", category = "1"))
  expect_error(get_player_recent_results(player = "", rank = "", category = "mens"))
  expect_error(get_player_recent_results(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_recent_results for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_player_recent_results("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_results(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_results("Raneem El Welily", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_results(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_results(rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
