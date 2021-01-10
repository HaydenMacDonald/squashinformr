context("Player Rankings History")

library(squashinformr)

test_that("test get_player_rankings_history for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## player and rank are both provided
  expect_error(get_player_rankings_history(player = 1, rank = 1, category = "mens"))

  ## player and rank are empty strings
  expect_error(get_player_rankings_history(player = "", category = "mens"))
  expect_error(get_player_rankings_history(rank = "", category = "mens"))
  expect_error(get_player_rankings_history(player = "", category = "womens"))
  expect_error(get_player_rankings_history(rank = "", category = "womens"))
  expect_error(get_player_rankings_history(player = "", category = "both"))
  expect_error(get_player_rankings_history(rank = "", category = "both"))

  ## rank is character, category is not valid
  expect_error(get_player_rankings_history(rank = "Mohamed Elshorbagy", category = "mens"))

  ## category is not valid
  expect_error(get_player_rankings_history(player = "Mohamed Elshorbagy", category = "1"))

  ## player and rank are both provided, but not valid
  expect_error(get_player_rankings_history(player = "", rank = "", category = "mens"))
  expect_error(get_player_rankings_history(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_rankings_history for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## category == "mens"
  df <- get_player_rankings_history(player = "Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == "womens"
  df <- get_player_rankings_history(player = "Nouran Gohar", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  ## category == NULL or "both"
  df <- get_player_rankings_history(player = "Mohamed Elshorbagy", rank = NULL, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history(player = NULL, rank = 1, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
