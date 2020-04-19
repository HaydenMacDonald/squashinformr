context("Historical Rankings")
library(squashinformr)

test_that("test get_historical_rankings for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## year is character
  expect_error(get_historical_rankings(year = "2018", month = "Jan", category = "mens", top = 2))

  ## month is not character
  expect_error(get_historical_rankings(year = 2018, month = 3, category = "mens", top = 2))

  ## category is not character
  expect_error(get_historical_rankings(year = 2018, month = "Jan", category = 1, top = 2))

  ## top is not numeric
  expect_error(get_historical_rankings(year = 2018, month = "Jan", category = "mens", top = "2"))

  ## category is not valid
  expect_error(get_historical_rankings(year = 2018, month = "Jan", category = "man", top = 2))


})

## Currently commented out because they take forever to run

test_that("test get_historical_rankings for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## category == "mens"
  #df <- get_historical_rankings(year = 2018, month = "Jan", category = "mens", top = 2)
  #expect_is(df, "data.frame")
  #expect_is(df, "tbl")
  #expect_equal(nrow(df), 2)

  ## category == "womens"
  #df <- get_historical_rankings(year = 2018, month = "Jan", category = "womens", top = 2)
  #expect_is(df, "data.frame")
  #expect_is(df, "tbl")
  #expect_equal(nrow(df), 2)

  ## category == "both"
  df <- get_historical_rankings(year = 2018, month = "Jan", category = "both", top = 2)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 4)

})
