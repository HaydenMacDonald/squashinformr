context("Matchup")

test_that("test get_matchup for wrong input errors", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  expect_error(get_matchup(player_1 = 1, player_2 = 2, category = "mens", tidy = TRUE))
  expect_error(get_matchup(player_1 = "", player_2 = "", category = "mens", tidy = TRUE))
  expect_error(get_matchup(player_1 = NULL, player_2 = NULL, category = "mens", tidy = TRUE))
  expect_error(get_matchup(player_1 = NULL, player_2 = NULL, ranks = 1, category = "mens", tidy = TRUE))
  expect_error(get_matchup("Mohamed Elshorbagy", "Ali Farag", ranks = 1:2, category = "mens", tidy = TRUE))
  expect_error(get_matchup("Raneem El Welily", "Nouran Gohar", ranks = 1:2, category = "womens", tidy = TRUE))
  expect_error(get_matchup("Mohamed Elshorbagy", "Raneem El Welily", category = "both", tidy = TRUE))
  expect_error(get_matchup("Mohamed Elshorbagy", ranks = 1:2, category = "mens", tidy = TRUE))
  expect_error(get_matchup("Mohamed Elshorbagy", "Ali Farag", ranks = 1, category = "mens", tidy = TRUE))
  expect_error(get_matchup("Mohamed Elshorbagy", "Ali Farag", "Karim Abdel Gawad", category = "mens", tidy = TRUE))

})

test_that("test get_matchup for proper outputs", {

  testthat::skip_if_offline()
  testthat::skip_on_cran()

  ## category == "mens"
  df <- get_matchup("Mohamed Elshorbagy", "Ali Farag", category = "mens", tidy = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 1)

  df <- get_matchup(ranks = 1:2, category = "mens", tidy = FALSE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 23)

  ## category == "womens"
  df <- get_matchup("Raneem El Welily", "Nouran Gohar", category = "womens", tidy = TRUE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 1)

  df <- get_matchup(ranks = 1:2, category = "womens", tidy = FALSE)
  expect_is(df, "data.frame")
  expect_is(df, "tbl")
  expect_equal(nrow(df), 23)

})
