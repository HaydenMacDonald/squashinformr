context("Player Recent Games")
library(squashinformr)

test_that("test get_player_recent_games for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Both player and rank are provided
  expect_error(get_player_recent_games(player = 1, rank = 1, category = "mens"))

  ## Multiple players provided
  expect_error(get_player_recent_games(player = c("Mohamed Elshorbagy", "Ali Farag"), rank = NULL, category = "mens"))

  ## Player and ranks are empty strings
  expect_error(get_player_recent_games(player = "", category = "mens"))
  expect_error(get_player_recent_games(rank = "", category = "mens"))
  expect_error(get_player_recent_games(player = "", category = "womens"))
  expect_error(get_player_recent_games(rank = "", category = "womens"))
  expect_error(get_player_recent_games(player = "", category = "both"))
  expect_error(get_player_recent_games(rank = "", category = "both"))

  ## Incorrect category provided
  expect_error(get_player_recent_games(rank = 1, category = "man"))

  ## Rank is character, category is not valid
  expect_error(get_player_recent_games(rank = "Mohamed Elshorbagy", category = "1"))

  ## Player and rank are both provided, but not valid
  expect_error(get_player_recent_games(player = "", rank = "", category = "mens"))
  expect_error(get_player_recent_games(player = NULL, rank = NULL, category = "mens"))

  ## "Do not provide player names when supplying multiple ranks"
  expect_error(get_player_recent_games(player = "Mohamed Elshorbagy", rank = 1:2, category = "mens"))

  ## "When scraping across competition categories, only provide ranks"
  expect_error(get_player_recent_games(player = "Mohamed Elshorbagy", rank = NULL, category = "both"))

})

test_that("test get_player_recent_games for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## category == "mens"
  df <- get_player_recent_games("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == "womens"
  df <- get_player_recent_games("Raneem El Welily", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_games(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == "both"
  df <- get_player_recent_games(player = NULL, rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$player)), 2)

  df <- get_player_recent_games(player = NULL, rank = 1:2, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$player)), 4)

})
