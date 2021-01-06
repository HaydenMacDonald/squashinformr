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
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom Hmisc %nin%
#' @importFrom utils tail
#'
#' @export


get_player_recent_results <- function(player = NULL, rank = NULL, category = NULL) {

  ## Stop if player is not type character or NULL, or if player is an empty string
  ## Stop if rank is not type numeric or NULL
  stopifnot(is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank))

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

    womens_profile_urls <- c()

  # Women's
  } else if (category == "womens") {

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = category)

    mens_profile_urls <- c()

  # Both categories
  } else if (category == "both" | is.null(category) == TRUE) {

    mens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "mens")

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "womens")

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }

  ## Event Summary Tables

  ## Men's Event Summary Tables
  ## If we have profile slugs for men...
  if (length(mens_profile_urls$profile_slugs) > 0) {

    ## Fetch Event Summary tables and assign to mens_recent_events
    mens_recent_events <- get_player_event_summary_table(mens_profile_urls)

  } else {

    ## Else define mens_recent_events as an empty vector
    mens_recent_events <- c()

  }


  ## Women's Event Summary Tables
  ## If we have profile slugs for women...
  if (length(womens_profile_urls$profile_slugs) > 0) {

    ## Fetch Event Summary tables and assign to womens_recent_events
    womens_recent_events <- get_player_event_summary_table(womens_profile_urls)

  } else {

    ## Else define womens_recent_events as an empty vector
    womens_recent_events <- c()

  }

  ## Create master data frame, recent_events, from combined men's and women's data frames before cleaning resulting data
  recent_events <- bind_rows(mens_recent_events, womens_recent_events) %>%
                        rename(country = ctry, event_date = date) %>%
                        mutate(seeding = str_extract(seeding, pattern = "[0-9]+"),
                               tour = if_else(tour == "-", NA_character_, tour),
                               round_reached = if_else(round_reached == "-", NA_character_, round_reached))


  recent_events <- recent_events %>%
                        select(rank, player, seeding, round_reached, event, event_date, country, tour)


  return(recent_events)

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
  ranking_table <- c()

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


#' Get Player Event Summary Tables from SquashInfo
#'
#' Given a data frame with player ranks, names, and profile slugs, \code{get_player_profile_urls()} returns the Event Summary Table of ranked players in PSA World Tour competitions.
#'
#' @param data data frame with columns for player rank (int), names (chr), and profile slugs (chr).
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

get_player_event_summary_table <- function(data) {

  ## Create recent_events
  recent_events <- c()

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
    recent_result <- suppressWarnings(recent_result[str_detect(recent_result, "Round Reached")][[1]]) %>%
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



