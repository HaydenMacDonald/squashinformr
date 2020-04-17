context("Player Recent Games")
library(squashinformr)

test_that("test get_player_recent_games for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_player_recent_games(player = 1, rank = 1, category = "mens"))
  expect_error(get_player_recent_games(player = "", category = "mens"))
  expect_error(get_player_recent_games(rank = "", category = "mens"))
  expect_error(get_player_recent_games(player = "", category = "womens"))
  expect_error(get_player_recent_games(rank = "", category = "womens"))
  expect_error(get_player_recent_games(player = "", category = "both"))
  expect_error(get_player_recent_games(rank = "", category = "both"))
  expect_error(get_player_recent_games(rank = "Mohamed Elshorbagy", category = "1"))
  expect_error(get_player_recent_games(player = "", rank = "", category = "mens"))
  expect_error(get_player_recent_games(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_recent_games for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_player_recent_games("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games("Raneem El Welily", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games(rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
