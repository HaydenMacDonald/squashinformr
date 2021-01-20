context("Tournaments")
library(squashinformr)

test_that("test get_tournaments for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## tournament is character
  expect_error(get_tournaments(year = "2020", world_tour = TRUE))

  ## world_tour is not logical
  expect_error(get_tournaments(year = 2020, world_tour = "TRUE"))

  ## year does not have 4 digits
  expect_error(get_tournaments(year = 20, world_tour = TRUE))

  ## year is negative
  expect_error(get_tournaments(year = -2020, world_tour = TRUE))

})

test_that("test get_tournaments for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- get_tournaments(year = 2021, world_tour = FALSE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$category)), 2)
  expect_is(sample(df$date, 1), "Date")
  expect_equal(year(sample(df$date, 1)), 2021)

  df <- get_tournaments(year = 2020, world_tour = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(length(unique(df$category)), 2)
  expect_is(sample(df$date, 1), "Date")
  expect_equal(year(sample(df$date, 1)), 2020)

})
