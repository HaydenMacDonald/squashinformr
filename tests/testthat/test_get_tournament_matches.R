context("Tournament Matches")

test_that("test get_tournament_matches for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(squashinformr::get_tournament_matches(1, year = 2020, world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches("CIB Egyptian Open", year = "2020-01-01", world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches("CIB Egyptian Open", year = 2020, world_tour = "TRUE"))
  expect_error(squashinformr::get_tournament_matches("Premier League", year = 2020, world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches("CIB Egyptian Open", year = 2018, world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches(year = 20, world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches(year = -2020, world_tour = TRUE))
  expect_error(squashinformr::get_tournament_matches(tournament = NULL, year = 2021, world_tour = FALSE))

})

test_that("test get_tournament_matches for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## Allam British Open, with year
  df <- squashinformr::get_tournament_matches("Allam British Open", year = 2022, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2022)

  ## Allam British Open, without year
  df <- squashinformr::get_tournament_matches("Allam British Open", year = NULL, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_is(sample(df$tournament_date, 1), "Date")
  expect_equal(year(sample(df$tournament_date, 1)), 2022)

})
