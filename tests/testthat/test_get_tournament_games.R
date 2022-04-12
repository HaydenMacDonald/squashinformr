context("Tournament Games")

test_that("test get_tournament_games for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## If integer provided to tournament
  expect_error(squashinformr::get_tournament_games(1, year = 2020, world_tour = TRUE))
  ## If date provided to year
  expect_error(squashinformr::get_tournament_games("CIB Egyptian Open", year = "2020-01-01", world_tour = TRUE))
  ## If string provided to world_tour
  expect_error(squashinformr::get_tournament_games("CIB Egyptian Open", year = 2020, world_tour = "TRUE"))
  ## If tournament is "Premier League"
  expect_error(squashinformr::get_tournament_games("Premier League", year = 2020, world_tour = TRUE))
  ## If tournament is beyond available data
  expect_error(squashinformr::get_tournament_games("CIB Egyptian Open", year = 2018, world_tour = TRUE))
  ## If integer provided to year
  expect_error(squashinformr::get_tournament_games(year = 20, world_tour = TRUE))
  ## If negative integer provided to year
  expect_error(squashinformr::get_tournament_games(year = -2020, world_tour = TRUE))
  ## If querying for more than 6 months of data
  expect_error(squashinformr::get_tournament_games(tournament = NULL, year = 2021, world_tour = FALSE))

})

test_that("test get_tournament_games for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Black Ball Open, with year
  df <- squashinformr::get_tournament_games("Allam British Open", year = 2022, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2022)

  ## Black Ball Open, without year
  df <- squashinformr::get_tournament_games("Allam British Open", year = NULL, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2022)

})
