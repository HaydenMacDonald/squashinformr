context("Player Recent Matches")

library(squashinformr)

test_that("test get_player_recent_matches for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Both player and rank are provided
  expect_error(get_player_recent_matches(player = 1, rank = 1, category = "mens"))

  ## Multiple players provided
  expect_error(get_player_recent_matches(player = c("Mohamed Elshorbagy", "Ali Farag"), rank = NULL, category = "mens"))

  ## Player and ranks are empty strings
  expect_error(get_player_recent_matches(player = "", category = "mens"))
  expect_error(get_player_recent_matches(rank = "", category = "mens"))
  expect_error(get_player_recent_matches(player = "", category = "womens"))
  expect_error(get_player_recent_matches(rank = "", category = "womens"))
  expect_error(get_player_recent_matches(player = "", category = "both"))
  expect_error(get_player_recent_matches(rank = "", category = "both"))

  ## Incorrect category provided
  expect_error(get_player_recent_matches(rank = 1, category = "man"))

  ## Rank is character, category is not valid
  expect_error(get_player_recent_matches(rank = "Mohamed Elshorbagy", category = "1"))

  ## Player and rank are both provided, but not valid
  expect_error(get_player_recent_matches(player = "", rank = "", category = "mens"))
  expect_error(get_player_recent_matches(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_recent_matches for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## category == "mens"
  df <- get_player_recent_matches("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_matches(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == "womens"
  df <- get_player_recent_matches("Raneem El Welily", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_matches(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == "both"
  df <- get_player_recent_matches(player = NULL, rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$player)), 2)

  df <- get_player_recent_matches(player = NULL, rank = 1:2, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$player)), 4)

})
