#' Get a player's recent event results from SquashInfo
#'
#' Given the full name or rank of a player and the competition category, \code{get_player_recent_results()} returns the recent event results table for PSA ranked players.
#'
#'
#' @param player character string of player name.
#'
#' @param rank single integer or vector of integers indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, player name, player's seeding, round reached, event name, event date, event location, and event tour.
#'
#' @examples
#'
#' ## Get Mohamed Elshorbagy's most recent results data
#' \donttest{get_player_recent_results(player = "Mohamed Elshorbagy", category = "mens")}
#'
#' ## Get Nour El Tayeb's recent results data
#' \donttest{get_player_recent_results("El Tayeb", category = "womens")}
#'
#' ## Get recent results data from the top two players in both Men's and Women's competitions
#' \donttest{get_player_recent_results(rank = 1:2, category = "both")}
#'
#' @note This function only returns results data from players ranked in the most recent PSA rankings table for Men's and Women's singles competitions. Recent results are limited to events that occurred within the current and previous calendar years.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/}
#'
#' @export

get_player_recent_results <- function(player = NULL, rank = NULL, category = NULL) {

  results <- get_player_recent_(level = "results", player = player, rank = rank, category = category)

  return(results)

}




#' Get a player's recent matches from SquashInfo
#'
#' Given the full name or rank of a player and the competition category, \code{get_player_recent_matches()} returns recent match data for PSA ranked players.
#'
#'
#' @param player character string of player name.
#'
#' @param rank single integer or vector of integers indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, name, opponent, match result, games won, games lost, match time, date, tournament round, event, PSA designation, and event location.
#'
#' @examples
#'
#' ## Get Mohamed Elshorbagy's most recent match data
#' \donttest{get_player_recent_matches(player = "Mohamed Elshorbagy", category = "mens")}
#'
#' ## Get Nour El Tayeb's recent match data
#' \donttest{get_player_recent_matches("El Tayeb", category = "womens")}
#'
#' ## Get recent match data from the top two players in both Men's and Women's competitions
#' \donttest{get_player_recent_matches(rank = 1:2, category = "both")}
#'
#' @note This function only returns data from players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @export

get_player_recent_matches <- function(player = NULL, rank = NULL, category = NULL) {

  matches <- get_player_recent_(level = "matches", player = player, rank = rank, category = category)

  return(matches)

}




#' Get a player's recent games from SquashInfo
#'
#' Given the full name or rank of a player and the competition category, \code{get_player_recent_games()} returns recent game data for PSA ranked players.
#'
#'
#' @param player character string of player name.
#'
#' @param rank single integer or vector of integers indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, name, opponent, points won, points lost, game result, tournament round, event, PSA designation, and event location.
#'
#' @examples
#'
#' ## Get Mohamed Elshorbagy's most recent game data
#' \donttest{get_player_recent_games(player = "Mohamed Elshorbagy", category = "mens")}
#'
#' ## Get Nour El Tayeb's recent game data
#' \donttest{get_player_recent_games("El Tayeb", category = "womens")}
#'
#' ## Get recent game data from the top two players in both Men's and Women's competitions
#' \donttest{get_player_recent_games(rank = 1:2, category = "both")}
#'
#' @note This function only returns data from players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @export

get_player_recent_games <- function(player = NULL, rank = NULL, category = NULL) {

  games <- get_player_recent_(level = "games", player = player, rank = rank, category = category)

  return(games)

}








#' Get a player's recent data from SquashInfo
#'
#' Given level of detail (e.g. 'results', 'matches', 'games'), the full name or rank of a player, and the competition category, \code{get_player_recent_()} returns the recent event data for PSA ranked players.
#'
#' @param level character string indicating the level of detail of the data to be returned. Must be one of "results", "matches", or "games".
#'
#' @param player character string of player name.
#'
#' @param rank single integer or vector of integers indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, player name, player's seeding, round reached, event name, event date, event location, and event tour.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_remove
#' @importFrom tidyr unnest

get_player_recent_ <- function(level = c("results", "matches", "games"), player = NULL, rank = NULL, category = NULL) {

  ## Stop if level is NULL
  ## Stop if player is not type character or NULL, or if player is an empty string
  ## Stop if rank is not type numeric or NULL
  stopifnot(is.null(level) == FALSE, is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank))

  ## If level is not one of the following, then return error
  if (level %nin% c("results", "matches", "games")) {
    stop("level must be one of 'results', 'matches', or 'games'")
  }

  ## If player AND rank are NULL then return error
  if (is.null(player) == TRUE & is.null(rank) == TRUE) {
    stop("Either a player's full name or rank is required")
  }

  ## If length of player is not one AND rank is NULL, then return error
  if (length(player) != 1 & is.null(rank)) {
    stop("A single player's full name is required")
  }

  ## If rank is not length 1 AND rank is numeric AND player is not NULL then return error
  if (length(rank) != 1 & is.numeric(rank) & is.null(player) == FALSE) {
    stop("Do not provide player names when supplying multiple ranks")
  }

  ## If querying both competition categories, only accept ranks
  if (category == "both") {
    if (is.null(player) == FALSE | is.null(rank) == TRUE) {
      stop("When scraping across competition categories, only provide ranks")
    }
  }

  ## Make category lowercase
  category <- tolower(category)

  # Men's
  if (category == "mens") {

    mens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = category)

    womens_profile_urls <- NULL

  # Women's
  } else if (category == "womens") {

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = category)

    mens_profile_urls <- NULL

  # Both categories
  } else if (category == "both" | is.null(category) == TRUE) {

    mens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "mens")

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "womens")

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


  if (level == "results") {

    ## Create master data frame, recent_events, from combined men's and women's data frames
    recent_events <- aggregate_profile_tables(x = mens_profile_urls, y = womens_profile_urls, identifier = "Round Reached")

    ## Clean resulting data
    recent_events <- recent_events %>%
      rename(country = ctry, event_date = date) %>%
      mutate(seeding = str_extract(seeding, pattern = "[0-9]+"),
             tour = if_else(tour == "-", NA_character_, tour),
             round_reached = if_else(round_reached == "-", NA_character_, round_reached))

    ## Select columns
    recent_events <- recent_events %>%
      select(rank, player, seeding, round_reached, event, event_date, country, tour)

    return(recent_events)



  } else if (level == "matches") {

    ## Create master data frame, recent_events, from combined men's and women's data frames
    recent_matches <- aggregate_profile_tables(x = mens_profile_urls, y = womens_profile_urls, identifier = "match_summary_table")

    ## Clean resulting data
    recent_matches <- recent_matches %>%
      rename(round = rnd, country = ctry, result = w_l) %>%
      mutate(round = toupper(round),
             match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))),  ## Extract match time
             match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
             score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""),  ## Extract score
             score = str_extract_all(score, pattern = "[0-9]+\\-[0-9]+"),
             ## Create games_won and games_lost variables
             games_won = NA_real_,
             games_lost = NA_real_)

    ## For each match
    for (j in seq_along(recent_matches$score)) {

      ## If there are no games, next row, create match object
      if (length(recent_matches$score[[j]]) == 0) { next } else { match <- recent_matches$score[[j]] }

      ## Start with 0 games won and games lost
      wins <- 0
      losses <- 0

      ## For each game in each match
      for (i in seq_along(match)) {
        ## If player 1's score is greater than player 2's score
        if (as.numeric(str_extract(match[i], pattern = "^[0-9]+")) > as.numeric(str_extract(match[i], pattern = "[0-9]+$"))) {
          ## Add one win to player 1's wins
          wins <- wins + 1
          ## If player 1's score is less than player 2's score
        } else {
          ## Add one loss to player 1's losses
          losses <- losses + 1
        }
      }

      ## Assign total wins and losses to games_won and games_lost, respectively
      recent_matches$games_won[j] <- wins
      recent_matches$games_lost[j] <- losses

    }


    ## Organize results
    recent_matches <- recent_matches %>%
      select(rank, player, opponent, result, games_won, games_lost, match_time, round, date, event, country, psa)

    return(recent_matches)



  } else if (level == "games") {

    ## Create master data frame, recent_events, from combined men's and women's data frames
    recent_games <- aggregate_profile_tables(x = mens_profile_urls, y = womens_profile_urls, identifier = "match_summary_table")

    ## Clean resulting data
    recent_games <- recent_games %>%
      rename(round = rnd, country = ctry, result = w_l) %>%
      mutate(round = toupper(round),
             match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))), ## Extract match time
             match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
             score = str_replace_all(score, pattern = regex(" \\([:digit:]{2,}m\\)"), replacement = ""),  ## Extract scores
             score = strsplit(score, ", ")) %>% ## Split scores across elements of a list
      unnest(score) %>% ## unnest lists into rows
      mutate(points_won = as.numeric(str_extract(score, "^[0-9]+")),  ## Extract points won
             points_lost = as.numeric(str_extract(score, "[0-9]+$")),  ## Extract points lost
             game_result = if_else(points_won > points_lost, "W", "L"))  ## Determine game result


    ## Organize results
    recent_games <- recent_games %>%
      select(rank, player, opponent, points_won, points_lost, game_result, round, event, tournament_date = date, country, psa)

    return(recent_games)

  }


}









#' Get Player URLs from SquashInfo
#'
#' Given the player name, rank(s), and competition category, \code{get_player_profile_urls()} returns profile slugs of ranked players in PSA World Tour competitions.
#'
#' @param player character string of player name.
#'
#' @param rank single integer or vector of integers indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#' @return Data frame containing player ranks, names, and profile slugs.
#'
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_table
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom Hmisc %nin%
#' @importFrom stringr str_detect
#' @importFrom utils tail

get_player_profile_urls <- function(player = NULL, rank = NULL, category = c("mens", "womens")) {

  ## Get profile URLs for top n players
  if (category == "mens") {
    rankings_url <- "http://www.squashinfo.com/rankings/men"
  } else if (category == "womens") {
    rankings_url <- "http://www.squashinfo.com/rankings/women"
  }

  ## Check URL for Robots.txt
  suppressMessages(session <- bow(rankings_url))

  ## Create ranking_table
  ranking_table <- NULL

  ## Rankings table url
  rankings_url <- paste0(rankings_url, "/1")

  if (is.null(player) == FALSE & is.null(rank) == TRUE) {

    while(TRUE %nin% str_detect(ranking_table$Name, player)) {

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

      ## Store data in ranking_table
      ranking_table <- rbind(ranking_table, results)

      # Find url in "Next" button
      rankings_url <- current_page %>%
        html_nodes("a")

      rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
        html_attr("href")

      ## Replace t_url with url in Next page button
      rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

    }

  } else if (is.null(player) == TRUE & is.null(rank) == FALSE) {

    while(tail(rank, n = 1) %nin% ranking_table$Rank) {

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

      ## Store data in ranking_table
      ranking_table <- rbind(ranking_table, results)

      # Find url in "Next" button
      rankings_url <- current_page %>%
        html_nodes("a")

      rankings_url <- suppressWarnings(rankings_url[str_detect(rankings_url, "Next")]) %>%
        html_attr("href")

      ## Replace t_url with url in Next page button
      rankings_url <- sprintf("http://www.squashinfo.com%s", rankings_url)

    }

  }

  ## If rank is null, filter by name
  if (is.null(rank) == TRUE) {

    profile_urls <- ranking_table %>%
      filter(str_detect(Name, player))

  } else if (is.null(player) == TRUE) { ## If player is null, filter by rank

    profile_urls <- ranking_table %>%
      filter(Rank %in% rank)
  }

  return(profile_urls)

}






#' Get Player Summary Tables from SquashInfo
#'
#' Given a data frame with player ranks, names, and profile slugs, \code{get_player_profile_urls()} returns the Event Summary Table of ranked players in PSA World Tour competitions.
#'
#' @param data data frame with columns for player rank (int), names (chr), and profile slugs (chr).
#'
#' @param identifier unique string that used to identify the desired table.
#'
#' @return Data frame containing Event Summary Table(s).
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr everything
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble

get_player_profile_table <- function(data = NULL, identifier = NULL) {

  ## Return error if either arguments are NULL
  stopifnot(is.null(data) == FALSE | is.data.frame(data) == TRUE, is.null(identifier) == FALSE | identifier %in% c("Round Reached", "match_summary_table"))

  ## Create recent_events
  recent_events <- NULL

  ## For each profile url...
  for (i in seq_along(data$profile_slugs)) {

    ## Extract player name
    player_name <- data$Name[i]

    ## Make function verbose with each iteration
    message("Scraping ", player_name, "'s recent results")

    ## Extract player rank
    Rank <- data$Rank[i]

    ## Attach profile slug to create player profile url
    profile_url <- sprintf("http://www.squashinfo.com%s", data$profile_slugs[i])

    ## Scrape profile
    current_page <- suppressMessages(bow(profile_url)) %>%
      scrape()

    ## Find tables in profile
    recent_result <- current_page %>%
      html_nodes("table")

    ## Find table with "Round Reached" column and clean it
    ## This column is the only unique id for the table, as the html table does not have a id tag
    recent_result <- suppressWarnings(recent_result[str_detect(recent_result, identifier)][[1]]) %>%
      html_table() %>%
      as_tibble() %>%
      filter(row_number() != n()) %>%
      clean_names() %>%
      mutate(player = player_name, ## insert player data...
             rank = Rank,
             date = ymd(parse_date_time(date, orders = "bY"))) %>%
      select(rank, player, everything())

    ## Bind recent_results with recent_events data frame
    recent_events <- rbind(recent_events, recent_result)

  }

  return(recent_events)

}






#' Aggregate Profile Tables from Player Profiles
#'
#' Given two data frames with profile urls, fetch and aggregate summary tables from player profiles.
#'
#' @param x,y data frames containing
#'
#' @param identifier unique string that used to identify the desired table.
#'
#' @return Data frame containing aggregated summary tables.

aggregate_profile_tables <- function(x = NULL, y = NULL, identifier = NULL) {

  ## If any are NULL, then return error
  if (is.null(x) == TRUE & is.null(y) == TRUE) {
    stop("x and y cannot both be null")
  }

  if (is.null(identifier) == TRUE) {
    stop("identifer cannot be null")
  }

  ## Summary Tables

  ## First set of summary tables
  ## If we have profile slugs...
  if (length(x$profile_slugs) > 0) {

    ## Fetch summary tables and assign to x_recent_events
    x_recent_events <- get_player_profile_table(x, identifier = identifier)

  } else {

    ## Else define x_recent_events as an empty vector
    x_recent_events <- NULL

  }


  ## Second set of summary tables
  ## If we have profile slugs...
  if (length(y$profile_slugs) > 0) {

    ## Fetch summary tables and assign to y_recent_events
    y_recent_events <- get_player_profile_table(y, identifier = identifier)

  } else {

    ## Else define y_recent_events as an empty vector
    y_recent_events <- NULL

  }

  ## Bind rows of data frames
  recent_events <- rbind(x_recent_events, y_recent_events)

  return(recent_events)

}



