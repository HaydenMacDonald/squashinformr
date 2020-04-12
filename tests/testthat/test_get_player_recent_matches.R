context("Player Recent Matches")

library(squashinformr)

test_that("test get_player_recent_matches for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_player_recent_matches(player = 1, rank = 1, category = "mens"))
  expect_error(get_player_recent_matches(player = "", category = "mens"))
  expect_error(get_player_recent_matches(rank = "", category = "mens"))
  expect_error(get_player_recent_matches(rank = "Mohamed Elshorbagy", category = "1"))
  expect_error(get_player_recent_matches(player = "", rank = "", category = "mens"))
  expect_error(get_player_recent_matches(player = NULL, rank = NULL, category = "mens"))

})

test_that("test get_player_recent_matches for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_player_recent_matches("Mohamed Elshorbagy", category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  df <- get_player_recent_matches(rank = 1, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")

})
