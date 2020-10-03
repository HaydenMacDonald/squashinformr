#' Get a player matchup data from SquashInfo
#'
#' Given the full names or ranks of players, and the competition category, \code{get_matchup()} returns recent matchup data for PSA ranked players.
#'
#' @param player_1 character string of the first player's name.
#'
#' @param player_2 character string of the second player's name.
#'
#' @param ranks integers indicating the rank of the PSA players to return.
#'
#' @param category character string indicating the competition category. Must be one of "mens" or "womens".
#'
#' @param tidy logical indicating whether to organize results according to tidy principles.
#'
#' @param match_spread logical indicating whether to only return match spread statistics.
#'
#'
#' @return Tibble containing each player's rank, name, total matches played, number of matches won, the match results spread (relative to player 1), the average match time, the number of games played, the number of games won, average point advantage in a won game, the average point difference in final scores, the number of tie-break wins, and the percentage of games that go to a tie-breaker.
#'
#' @examples
#'
#' ## Get tidy matchup data for Mohamed Elshorbagy vs Ali Farag
#' \donttest{get_matchup(player_1 = "Mohamed Elshorbagy", player_2 = "Ali Farag", category = "mens", tidy = TRUE)}
#'
#' ## Get non-tidy matchup data for Nouran Gohar vs Camille Serme
#' \donttest{get_matchup("Nouran Gohar", "Camille Serme", category = "womens", tidy = FALSE)}
#'
#' ## Get tidy match spread data for Paul Coll and Diego Elias
#' \donttest{get_matchup("Paul Coll", "Diego Elias", category = "mens", tidy = TRUE, match_spread = TRUE)}
#'
#'
#' @note This function only returns data from players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_longer
#' @importFrom plyr round_any
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr regex
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom Hmisc %nin%
#'
#' @export

get_matchup <- function(player_1 = NULL, player_2 = NULL, ranks = NULL, category = NULL, tidy = FALSE, match_spread = FALSE) {

  ## Input errors
  stopifnot(is.character(player_1) | is.null(player_1), is.character(player_2) | is.null(player_2))

  if (!(is.null(player_1) & is.null(player_2))) {

    players <- c(player_1, player_2)

  } else {

    players <- NULL

  }

  stopifnot(is.character(players) | is.null(players), nchar(players) > 0, is.numeric(ranks) | is.null(ranks))

  if (is.null(players) == TRUE & is.null(ranks) == TRUE) {

    stop("Either the players' full names or ranks are required")

  }


  if (category == "mens" | category == "womens") {

    if (length(ranks) != 2 & is.null(players) | length(players) != 2 & is.null(ranks)) {

      stop("Either two player names or two ranks are required")

    }

  }


  ## Make category lowercase
  category <- tolower(category)


  if (category == "mens") {

    # Men profile slugs

    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    mens_ranking_table <- c()

    ## Rankings table url
    rankings_url <- "http://www.squashinfo.com/rankings/men/1"

    if (is.null(players) == FALSE & is.null(ranks) == TRUE) {

      while(sum(str_detect(mens_ranking_table$Name, players), na.rm = TRUE) != 2) {

        ## Verbose
        message("Scraping ", rankings_url)

        ## Scrape table for player rank and name
        current_page <- suppressMessages(bow(rankings_url)) %>%
                                            scrape()

        results <- current_page %>%
                      html_nodes("table") %>%
                      html_table() %>%
                      as.data.frame() %>%
                      select(Rank, Name)

        ## Scrape table for player profile hrefs
        profile_slugs <- current_page %>%
                            html_nodes(xpath = "//td/a") %>%
                            html_attr("href")

        ## Combine player rank, name, and profile slug
        results <- cbind(results, profile_slugs) %>%
                                          as.data.frame()

        ## Store data in mens_ranking_table
        mens_ranking_table <- rbind(mens_ranking_table, results)

        # Find url in "Next" button
        rankings_url <- current_page %>%
                              html_nodes("a")

        rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
                                html_attr("href")

        ## Replace t_url with url in Next page button
        rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

      }

    } else if (is.null(players) == TRUE & is.null(ranks) == FALSE) {

      while(ranks[1] %nin% mens_ranking_table$Rank & ranks[2] %nin% mens_ranking_table$Rank) {

        ## Verbose
        message("Scraping ", rankings_url)

        ## Scrape table for player rank and name
        current_page <- suppressMessages(bow(rankings_url)) %>%
                                          scrape()

        results <- current_page %>%
                      html_nodes("table") %>%
                      html_table() %>%
                      as.data.frame() %>%
                      select(Rank, Name)

        ## Scrape table for player profile hrefs
        profile_slugs <- current_page %>%
                            html_nodes(xpath = "//td/a") %>%
                            html_attr("href")

        ## Combine player rank, name, and profile slug
        results <- cbind(results, profile_slugs) %>%
                                        as.data.frame()

        ## Store data in mens_ranking_table
        mens_ranking_table <- rbind(mens_ranking_table, results)

        # Find url in "Next" button
        rankings_url <- current_page %>%
                              html_nodes("a")

        rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
                              html_attr("href")

        ## Replace t_url with url in Next page button
        rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

      }

    }

    mens_profile_urls <- mens_ranking_table %>%
                                filter(if (is.null(ranks)) {str_detect(Name, players)} else if (is.null(players)) {Rank %in% ranks})

    womens_profile_urls <- c()



  } else if (category == "womens") {

    # Women slugs

    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url, verbose = FALSE))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    ## Rankings table url
    rankings_url <- "http://www.squashinfo.com/rankings/women/1"

    if (is.null(players) == FALSE & is.null(ranks) == TRUE) {

      while(sum(str_detect(womens_ranking_table$Name, players), na.rm = TRUE) != 2) {

        ## Verbose
        message("Scraping ", rankings_url)

        ## Scrape table for player rank and name
        current_page <- suppressMessages(bow(rankings_url)) %>%
                                            scrape()

        results <- current_page %>%
                      html_nodes("table") %>%
                      html_table() %>%
                      as.data.frame() %>%
                      select(Rank, Name)

        ## Scrape table for player profile hrefs
        profile_slugs <- current_page %>%
                            html_nodes(xpath = "//td/a") %>%
                            html_attr("href")

        ## Combine player rank, name, and profile slug
        results <- cbind(results, profile_slugs) %>%
                                        as.data.frame()

        ## Store data in mens_ranking_table
        womens_ranking_table <- rbind(womens_ranking_table, results)

        # Find url in "Next" button
        rankings_url <- current_page %>%
                              html_nodes("a")

        rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
                                  html_attr("href")

        ## Replace t_url with url in Next page button
        rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

      }

    } else if (is.null(players) == TRUE & is.null(ranks) == FALSE) {

      while(ranks[1] %nin% womens_ranking_table$Rank & ranks[2] %nin% womens_ranking_table$Rank) {

        ## Verbose
        message("Scraping ", rankings_url)

        ## Scrape table for player rank and name
        current_page <- suppressMessages(bow(rankings_url)) %>%
                                          scrape()

        results <- current_page %>%
                      html_nodes("table") %>%
                      html_table() %>%
                      as.data.frame() %>%
                      select(Rank, Name)

        ## Scrape table for player profile hrefs
        profile_slugs <- current_page %>%
                            html_nodes(xpath = "//td/a") %>%
                            html_attr("href")

        ## Combine player rank, name, and profile slug
        results <- cbind(results, profile_slugs) %>%
                                          as.data.frame()

        ## Store data in mens_ranking_table
        womens_ranking_table <- rbind(womens_ranking_table, results)

        # Find url in "Next" button
        rankings_url <- current_page %>%
                              html_nodes("a")

        rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
                                  html_attr("href")

        ## Replace t_url with url in Next page button
        rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

      }

    }

    womens_profile_urls <- womens_ranking_table %>%
                                    filter(if (is.null(ranks)) {str_detect(Name, players)} else if (is.null(players)) {Rank %in% ranks})

    mens_profile_urls <- c()


  } else {

    stop("category must be one of 'mens' or 'womens'")


  }


  ## Recent Matches

  ## Men's results

  if (length(mens_profile_urls$profile_slugs) > 0) { ## If we have more than 0 men's profile slugs

    ## Create mens_recent_matches dataframe
    mens_recent_matches <- c()

    for (i in seq_along(mens_profile_urls$profile_slugs)) { ## For every men's profile slug

      ## Extract player name
      player_name <- mens_profile_urls$Name[i]

      ## Extract current rank
      Rank <- mens_profile_urls$Rank[i]

      ## Verbose
      message("Scraping ", player_name, "'s profile")

      ## Create profile url from slug
      profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

      ## Bow and scrape page
      current_page <- suppressMessages(bow(profile_url)) %>%
                                        scrape()

      ## Find recent results table
      recent_result <- current_page %>%
                          html_nodes("table") %>%
                          .[[4]] %>%
                          html_table() %>%
                          filter(row_number() != n()) %>%
                          as_tibble() %>%
                          clean_names() %>%
                          mutate(player = player_name,
                                 rank = Rank,
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>% ## convert date to yyyy-mm-dd
                          select(rank, player, everything())

      ## Join data with opponent's data
      recent_result <- recent_result %>%
                          left_join(mens_profile_urls[,-3], by = c("opponent" = "Name")) %>%
                          rename(opponent_rank = Rank)

      ## Bind results row-wise
      mens_recent_matches <- rbind(mens_recent_matches, recent_result)

    }



  } else { ## If we have 0 men's profile slugs

    mens_recent_matches <- c() ## Create empty mens_recent_matches dataframe

  }


  ## Women's results

  if (length(womens_profile_urls$profile_slugs) > 0) { ## If we have more than 0 women's profile slugs

    ## Create womens_recent_matches dataframe
    womens_recent_matches <- c()

    for (i in seq_along(womens_profile_urls$profile_slugs)) { ## For every women's profile slug

      ## Extract player name
      player_name <- womens_profile_urls$Name[i]

      ## Extract current rank
      Rank <- womens_profile_urls$Rank[i]

      ## Verbose
      message("Scraping ", player_name, "'s profile")

      ## Create profile url from slug
      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

      ## Bow and scrape page
      current_page <- suppressMessages(bow(profile_url)) %>%
                                        scrape()

      ## Find recent results table
      recent_result <- current_page %>%
                          html_nodes("table") %>%
                          .[[4]] %>%
                          html_table() %>%
                          filter(row_number() != n()) %>%
                          as_tibble() %>%
                          clean_names() %>%
                          mutate(player = player_name,
                                 rank = Rank,
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>%
                          select(rank, player, everything())

      ## Join with opponent data
      recent_result <- recent_result %>%
                          left_join(womens_profile_urls[,-3], by = c("opponent" = "Name")) %>%
                          rename(opponent_rank = Rank)

      ## Bind results row-wise
      womens_recent_matches <- rbind(womens_recent_matches, recent_result)

    }




  } else { ## If we have 0 women's profile slugs

    womens_recent_matches <- c() ## Create empty womens_recent_matches dataframe

  }




  if (is.null(ranks)) { ## If ranks are not provided

    ## Bind men's and women's results
    recent_matches <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
                        filter(str_detect(player, players[1]) & str_detect(opponent, players[2])) %>%  ## Find players in results
                        rename(round = rnd, country = ctry, result = w_l) %>%
                        mutate(round = toupper(round),
                               match = row_number(),
                               match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))),  ## Extract match time
                               match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                               score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""),  ## Extract scores
                               score = str_extract_all(score, pattern = "[0-9]+\\-[0-9]+"),
                               ## Create games_won and games_lost variables
                               games_won = NA_real_,
                               games_lost = NA_real_)

    ## For each match
    for (j in seq_along(recent_matches$score)) {

      ## if the number of games equals 0, next row, else creat match variable
      if (length(recent_matches$score[[j]]) == 0) { next } else { match <- recent_matches$score[[j]] }

      ## Start with 0 wins and 0 losses
      wins <- 0

      losses <- 0

      ## For each game in each match
      for (i in seq_along(match)) {

        ## If player 1's score is greater than player 2's score
        if (as.numeric(str_extract(match[i], pattern = "^[0-9]+")) > as.numeric(str_extract(match[i], pattern = "[0-9]+$"))) {

          ## Add one game win for player 1
          wins <- wins + 1

          ## If player 1's score is less than player 2's score
        } else {

          ## Add one game loss for player 1
          losses <- losses + 1

        }


      }

      ## Assign total wins and losses to games_won and games_lost, respectively
      recent_matches$games_won[j] <- wins

      recent_matches$games_lost[j] <- losses

    }

    ## Organize recent matches
    recent_matches <- recent_matches %>%
                          select(match, rank, player, opponent_rank, opponent, games_won, games_lost, result, match_time, round, event, tournament_date = date, country, psa)

    ## Create recent games from men's and women's recent matches
    recent_games <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
                        filter(str_detect(player, players[1]) & str_detect(opponent, players[2])) %>%  ## Find players in results
                        rename(round = rnd, country = ctry, result = w_l) %>%
                        mutate(round = toupper(round),
                               match = row_number(),
                               match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))), ## Extract match time
                               match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                               score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""),  ## Extract scores
                               score = strsplit(score, ", ")) %>% ## Split scores into elements of a list
                        unnest(score) %>% ## unnest scores list across rows
                        mutate(points_won = as.numeric(str_extract(score, "^[0-9]+")), ## Extract points won
                               points_lost = as.numeric(str_extract(score, "[0-9]+$")), ## Extract points lost
                               game_result = if_else(points_won > points_lost, "W", "L")) ## Determine game winner

    ## Organize results
    recent_games <- recent_games %>%
                        select(rank, player, opponent_rank, opponent, points_won, points_lost, game_result, round, event, tournament_date = date, country, psa)



  } else if (is.null(players)) { ## If player names are not provided

    ## Bind results row-wise
    recent_matches <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
                      filter(rank %in% ranks[1] & opponent_rank %in% ranks[2])  %>%  ## Find players in results
                      rename(round = rnd, country = ctry, result = w_l) %>%
                      mutate(round = toupper(round),
                             match = n() + 1 - row_number(),
                             match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))), ## Extract match time
                             match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                             score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""), ## Extract scores
                             score = str_extract_all(score, pattern = "[0-9]+\\-[0-9]+"),
                             ## Create games_won and games_lost variables
                             games_won = NA_real_,
                             games_lost = NA_real_)

    ## For each match
    for (j in seq_along(recent_matches$score)) {

      ## If there are no games, next row, else create match object
      if (length(recent_matches$score[[j]]) == 0) { next } else { match <- recent_matches$score[[j]] }

      ## Start with 0 game wins and 0 game losses
      wins <- 0

      losses <- 0

      ## For every game in every match
      for (i in seq_along(match)) {

        ## If player 1's score is greater than player 2's score
        if (as.numeric(str_extract(match[i], pattern = "^[0-9]+")) > as.numeric(str_extract(match[i], pattern = "[0-9]+$"))) {

          ## Add one game win for player 1
          wins <- wins + 1

          ## If player 1's score is less than player 2's score
        } else {

          ## Add one game loss for player 1
          losses <- losses + 1

        }


      }


      ## Assign total wins and losses to games_won and games_lost, respectively
      recent_matches$games_won[j] <- wins

      recent_matches$games_lost[j] <- losses

    }

    ## Organize results
    recent_matches <- recent_matches %>%
                        select(match, rank, player, opponent_rank, opponent, games_won, games_lost, result, match_time, round, event, tournament_date = date, country, psa)

    ## Create recent games
    recent_games <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
                        filter(rank %in% ranks[1] & opponent_rank %in% ranks[2]) %>%  ## Find players in results
                        rename(round = rnd, country = ctry, result = w_l) %>%
                        mutate(round = toupper(round),
                               match = n() + 1 - row_number(),
                               match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))), ## Extract match time
                               match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                               score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""), ## Extract scores
                               score = strsplit(score, ", ")) %>% ## Split scores into elements of a list
                        unnest(score) %>%  ## unnest scores list across rows
                        mutate(points_won = as.numeric(str_extract(score, "^[0-9]+")),  ## Extract points won
                               points_lost = as.numeric(str_extract(score, "[0-9]+$")),  ## Extract points lost
                               game_result = if_else(points_won > points_lost, "W", "L"))  ## Determine game_result

    ## Organize results
    recent_games <- recent_games %>%
                        select(rank, player, opponent_rank, opponent, points_won, points_lost, game_result, round, event, tournament_date = date, country, psa)

  }


  ## Summarize Match Data
  recent_matches <- recent_matches %>%
                        rename(player_1_rank = rank,
                               player_1 = player,
                               player_2_rank = opponent_rank,
                               player_2 = opponent) %>%
                        summarize(player_1_rank = max(player_1_rank, na.rm = TRUE),
                                  player_1 = max(player_1),
                                  player_2_rank = max(player_2_rank, na.rm = TRUE),
                                  player_2 = max(player_2),
                                  matches_played = n(), ## Number of matches played
                                  player_1_matches_won = sum(result == "W", na.rm = TRUE), ## Number of matches won
                                  player_2_matches_won = sum(result == "L", na.rm = TRUE),
                                  matches_3_2 = sum(games_won == 3 & games_lost == 2, na.rm = TRUE),
                                  matches_3_1 = sum(games_won == 3 & games_lost == 1, na.rm = TRUE),
                                  matches_3_0 = sum(games_won == 3 & games_lost == 0, na.rm = TRUE),
                                  matches_0_3 = sum(games_won == 0 & games_lost == 3, na.rm = TRUE),  ## Match spread variables relative to player 1
                                  matches_1_3 = sum(games_won == 1 & games_lost == 3, na.rm = TRUE),
                                  matches_2_3 = sum(games_won == 2 & games_lost == 3, na.rm = TRUE),
                                  avg_match_time = mean(match_time, na.rm = TRUE)) ## Average match time


  ## Summarize Game Data
  recent_games <- recent_games %>%
                        rename(player_1_rank = rank,
                               player_1 = player,
                               player_2_rank = opponent_rank,
                               player_2 = opponent) %>%
                        mutate(point_diff = abs(points_won - points_lost),
                               player_1_advantage = if_else(points_won > points_lost, points_won - points_lost, NA_real_),  ## Player's point advantage in a won game
                               player_2_advantage = if_else(points_won < points_lost, points_lost - points_won, NA_real_)) %>%
                        summarize(player_1_rank = max(player_1_rank, na.rm = TRUE),
                                  player_1 = max(player_1),
                                  player_2_rank = max(player_2_rank, na.rm = TRUE),
                                  player_2 = max(player_2),
                                  games_played = n(),  ## Number of games played
                                  player_1_games_won = sum(game_result == "W", na.rm = TRUE),  # Games won
                                  player_2_games_won = sum(game_result == "L", na.rm = TRUE),
                                  player_1_avg_advantage = round(mean(player_1_advantage, na.rm = TRUE), 2),  ## Average advantage
                                  player_2_avg_advantage = round(mean(player_2_advantage, na.rm = TRUE), 2),
                                  avg_point_diff = round(mean(point_diff, na.rm = TRUE), 2),  ## Average point difference in all games
                                  player_1_tiebreak_wins = sum(points_won > 11, na.rm = TRUE), ## Tie-breaker wins
                                  player_2_tiebreak_wins = sum(points_lost > 11, na.rm = TRUE),
                                  pct_games_tiebreak = round(sum(points_won > 11 | points_lost > 11, na.rm = TRUE) / n() * 100, 2))  ## percent of games that go to tie-breaker


  ## Join recent_matches and recent_games to get full head-to-head comparison
  matchup <- recent_matches %>%
                  left_join(recent_games, by = c("player_1_rank" = "player_1_rank",
                                                 "player_1" = "player_1",
                                                 "player_2_rank" = "player_2_rank",
                                                 "player_2" = "player_2"))


  ## If match_spread == TRUE then only return spread statistics
  if (match_spread == TRUE) {

    matchup <- matchup %>%
                  select(matches_3_2, matches_3_1, matches_3_0, matches_0_3, matches_1_3, matches_2_3)

  }

  ## If tidy == FALSE, then provide matchup stats in long table format
  if (tidy == FALSE) {

    matchup <- matchup %>%
                  gather(key = "metric", value = "value")  ## Gather is deprecated / retired but pivot_longer did not allow me to create this same table...

  }


  return(matchup)


}
