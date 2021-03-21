context("Tournament Matches")

library(squashinformr)

test_that("test get_tournament_matches for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_tournament_matches(1, year = 2020, world_tour = TRUE))
  expect_error(get_tournament_matches("Black Ball Open", year = "2020-01-01", world_tour = TRUE))
  expect_error(get_tournament_matches("Black Ball Open", year = 2020, world_tour = "TRUE"))
  expect_error(get_tournament_matches("Premier League", year = 2020, world_tour = TRUE))
  expect_error(get_tournament_matches("Black Ball Open", year = 2018, world_tour = TRUE))
  expect_error(get_tournament_matches(year = 20, world_tour = TRUE))
  expect_error(get_tournament_matches(year = -2020, world_tour = TRUE))

})

test_that("test get_tournament_matches for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Black Ball Open, with year
  df <- get_tournament_matches("Black Ball Open", year = 2021, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$category)), 2)
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2021)

  ## Black Ball Open, without year
  df <- get_tournament_matches("Black Ball Open", year = NULL, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$category)), 2)
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2021)

  ## All 2020 tournaments
  df <- get_tournament_matches(tournament = NULL, year = 2021, world_tour = FALSE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$category)), 3)
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2021)

})
