context("Rankings")

test_that("test get_rankings for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(squashinformr::get_rankings(top = "", category = "mens"))
  expect_error(squashinformr::get_rankings(top = 1, category = ""))
  expect_error(squashinformr::get_rankings(rank = 1, category = "man"))
  expect_error(squashinformr::get_rankings(rank = 1, category = "woman"))

})

test_that("test get_rankings for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  df <- squashinformr::get_rankings(top = 5, category = "mens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 5)

  df <- squashinformr::get_rankings(top = 5, category = "womens")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 5)

  df <- squashinformr::get_rankings(top = 5, category = "both")
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 10)

})
