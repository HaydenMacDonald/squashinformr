context("Player Rankings History")

library(squashinformr)

test_that("test get_player_rankings_history for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_player_rankings_history(player = 1, rank = 1, category = "mens"))
  expect_error(get_player_rankings_history(player = "", category = "mens"))
  expect_error(get_player_rankings_history(rank = "", category = "mens"))
  expect_error(get_player_rankings_history(player = "", category = "womens"))
  expect_error(get_player_rankings_history(rank = "", category = "womens"))
  expect_error(get_player_rankings_history(player = "", category = "both"))
  expect_error(get_player_rankings_history(rank = "", category = "both"))
  expect_error(get_player_rankings_history(rank = "Mohamed Elshorbagy", category = "1"))
  expect_error(get_player_rankings_history(player = "", rank = "", category = "mens"))
  expect_error(get_player_rankings_history(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_rankings_history for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_player_rankings_history("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history("Raneem El Welily", category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_rankings_history(rank = 1, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
