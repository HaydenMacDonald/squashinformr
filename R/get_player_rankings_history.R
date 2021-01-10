#' Get a player's PSA ranking history from SquashInfo
#'
#' Given a player name or rank, and a competition category, \code{get_player_rankings_history()} returns a tidy version of the PSA rankings history table for that player(s).
#'
#' @param player character string of player name(s).
#' @param rank integer indicating the rank of the PSA player(s) to return.
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#' @return Tibble containing the year, month, exact date, rank, player name and player's current rank.
#'
#' @examples
#'
#' ## Get the rankings history for the top two men's singles players
#' \donttest{top_two <- get_player_rankings_history(rank = 1:2, category = "mens")}
#'
#' ## Get the rankings history for the top three women's singles players
#' \donttest{top_three <- get_player_rankings_history(rank = 1:3, category = "womens")}
#'
#' @note This function only returns PSA ranking histories for players currently ranked in PSA Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#'
#' @export

get_player_rankings_history <- function(player = NULL, rank = NULL, category = NULL) {

  stopifnot(is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank), is.character(category) | is.null(category))

  if (is.null(player) == TRUE & is.null(rank) == TRUE) {

    stop("Either a player's full name or rank is required")

  }

  category <- tolower(category)


  if (category == "mens") {

    mens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = category)

    womens_profile_urls <- c()

    ## Combine men's and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)


  } else if (category == "womens") {

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = category)

    mens_profile_urls <- c()

    ## Combine men's and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)


  } else if (category == "both") {

    mens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "mens")

    womens_profile_urls <- get_player_profile_urls(player = player, rank = rank, category = "womens")

    ## Combine men's and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }

  rankings_history <- aggregate_rankings_histories(x = all_profile_urls)

  return(rankings_history)

}




#' Get a player's PSA ranking history from SquashInfo
#'
#' Given a player name or rank, and a competition category, \code{get_player_rankings_history()} returns a tidy version of the PSA rankings history table for that player(s).
#'
#' @param player character string of player name(s).
#' @param rank integer indicating the rank of the PSA player(s) to return.
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#' @return Tibble containing the year, month, exact date, rank, player name and player's current rank.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom plyr round_any
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_replace

get_player_profile_urls <- function(player = NULL, rank = NULL, category = NULL) {

  # Get profile slugs

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

  for (i in if (is.null(rank) == TRUE) {1:10} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

    ## Next tab in rankings table
    rankings_url <- sprintf(paste0(rankings_url, "/%s"), i)

    current_page <- suppressMessages(bow(rankings_url)) %>%
      scrape()

    ## Scrape table for player rank and name
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

  }

  ## Profile urls
  profile_urls <- ranking_table %>%
      filter(Name %in% player | Rank %in% rank) %>%
      mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

  return(profile_urls)

}




#' Scrape and aggregate player rankings histories
#'
#' @param x a player data frame containing player name, player rank, and profile slug.
#'
#' @return A tibble containing the historical rankings of players provided.
#'
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr if_else
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time

aggregate_rankings_histories <- function(x) {

  ## Rankings History

  rankings_history <- c()

  for (i in seq_along(x$profile_slugs)) {

    ## Player's name
    player_name <- x$Name[i]

    ## Verbose
    message("Scraping ", player_name, "'s ranking history")

    ## Player's rank
    current_rank <- x$Rank[i]

    ## Ranking history table url
    rankings_history_url <- sprintf("http://www.squashinfo.com%s", x$profile_slugs[i])

    ## Bow and scrape
    current_page <- suppressMessages(bow(rankings_history_url)) %>%
      scrape()

    ## Find html tables
    result <- current_page %>%
      html_nodes("table")

    ## Extract the rankings history table
    result <- suppressWarnings(result[str_detect(result, "Year")][[1]]) %>%
      html_table() %>%
      clean_names() %>%
      as_tibble()

    ## vars <- c("year", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

    result_df <- result %>%
      mutate(across(2:length(.), as.character)) %>%
      pivot_longer(cols = !1, names_to = "month", values_to = "rank") %>%
      mutate(name = player_name,
             current_rank = current_rank,
             rank = if_else(rank == "-", NA_character_, rank), #replace hypens with NAs
             rank = as.numeric(rank), ## make rank numeric
             month = factor(month,
                            levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
                            labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), ## make month an ordered factor
             ## Create an exact date for each ranking (yyyy-mm-dd)
             exact_date = paste(month, year),
             exact_date = ymd(parse_date_time(exact_date, orders = "bY"))) %>%
      select(year, month, exact_date, rank, name, current_rank) %>%
      arrange(year, month)

    ## Bind results row-wise
    rankings_history <- bind_rows(rankings_history, result_df)

  }

  return(rankings_history)

}

