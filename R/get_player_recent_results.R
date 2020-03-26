#' Get a player's recent event results from SquashInfo
#'
#' Given the full name or rank of a player and the competition category, \code{get_player_recent_results()} returns the recent event results table for PSA ranked players.
#'
#'
#' @param player character string of player name(s).
#'
#' @param rank integer indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, player name, player's seeding, round reached, event name, event date, event location, and event tour.
#'
#' @examples
#'
#' ## Get Mohamed Elshorbagy's most recent results data
#' get_player_recent_results(player = "Mohamed Elshorbagy", category = "men")
#'
#' ## Get recent results data from the 4th ranked player in Women's competitions
#' get_player_recent_results(rank = 4, category = "womens")
#'
#' ## Get recent results data for Ali Farag and Nour El Tayeb
#' get_player_recent_results(player = c("Ali Farag", "Nour El Tayeb"), category = "both")
#'
#' ## Get recent results data from the top 5 players in both Men's and Women's competitions
#' get_player_recent_results(rank = 1:5, category = "both")
#'
#' @note This function only returns results data from players ranked in the most recent PSA rankings table for Men's and Women's singles competitions. Recent results are limited to events that occurred within the current and previous calendar years.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/}
#'
#' @import tibble
#' @import rvest
#' @import httr
#' @import xml2
#' @import polite
#' @importFrom plyr round_any
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @export


get_player_recent_results <- function(player = NULL, rank = NULL, category = NULL) {

  stopifnot(is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank))

  if (is.null(player) == TRUE & is.null(rank) == TRUE) {

    stop("Either a player's full name or rank is required")

  }

  category <- tolower(category)


  if (category == "mens") {

    # Men profile slugs

    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    mens_ranking_table <- c()

    for (i in if (is.null(rank) == TRUE) {1:10} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)

      ## Scrape table for player rank and name
      results <- read_html(rankings_url) %>%
                    html_nodes("table") %>%
                    html_table() %>%
                    as.data.frame() %>%
                    select(Rank, Name)

      ## Scrape table for player profile hrefs
      profile_slugs <- read_html(rankings_url) %>%
                          html_nodes(xpath = "//td/a") %>%
                          html_attr("href")

      ## Combine player rank, name, and profile slug
      results <- cbind(results, profile_slugs) %>%
                                      as.data.frame()

      ## Store data in mens_ranking_table
      mens_ranking_table <- rbind(mens_ranking_table, results)

    }

    mens_profile_urls <- mens_ranking_table %>%
                                  filter(Name %in% player | Rank %in% rank)

    womens_profile_urls <- c()



  } else if (category == "womens") {

    # Women slugs

    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url, verbose = FALSE))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    for (i in if (is.null(rank) == TRUE) {1:8} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)

      ## Scrape table for player profile hrefs
      results <- read_html(rankings_url) %>%
                    html_nodes("table") %>%
                    html_table() %>%
                    as.data.frame() %>%
                    select(Rank, Name)

      ## Scrape table for player profile hrefs
      profile_slugs <- read_html(rankings_url) %>%
                          html_nodes(xpath = "//td/a") %>%
                          html_attr("href")

      results <- cbind(results, profile_slugs) %>%
                                      as.data.frame()

      ## Store hrefs in
      womens_ranking_table <- rbind(womens_ranking_table, results)

    }

    womens_profile_urls <- womens_ranking_table %>%
                                      filter(Name %in% player | Rank %in% rank)

    mens_profile_urls <- c()



  } else if (category == "both" | is.null(category) == TRUE) {

    # Men profile slugs

    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    mens_ranking_table <- c()

    for (i in if (is.null(rank) == TRUE) {1:10} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)

      ## Scrape table for player rank and name
      results <- read_html(rankings_url) %>%
                    html_nodes("table") %>%
                    html_table() %>%
                    as.data.frame() %>%
                    select(Rank, Name)

      ## Scrape table for player profile hrefs
      profile_slugs <- read_html(rankings_url) %>%
                          html_nodes(xpath = "//td/a") %>%
                          html_attr("href")

      ## Combine player rank, name, and profile slug
      results <- cbind(results, profile_slugs) %>%
                                        as.data.frame()

      ## Store data in mens_ranking_table
      mens_ranking_table <- rbind(mens_ranking_table, results)

    }

    mens_profile_urls <- mens_ranking_table %>%
                                  filter(Name %in% player | Rank %in% rank)

    # Women slugs

    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url, verbose = FALSE))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    for (i in if (is.null(rank) == TRUE) {1:8} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)

      ## Scrape table for player profile hrefs
      results <- read_html(rankings_url) %>%
                    html_nodes("table") %>%
                    html_table() %>%
                    as.data.frame() %>%
                    select(Rank, Name)

      ## Scrape table for player profile hrefs
      profile_slugs <- read_html(rankings_url) %>%
                          html_nodes(xpath = "//td/a") %>%
                          html_attr("href")

      results <- cbind(results, profile_slugs) %>%
                                      as.data.frame()

      ## Store hrefs in
      womens_ranking_table <- rbind(womens_ranking_table, results)

    }

    womens_profile_urls <- womens_ranking_table %>%
                                    filter(Name %in% player | Rank %in% rank)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")


  }







## Recent Results

  ## Men's recent results

  if (length(mens_profile_urls$profile_slugs) > 0) {

    mens_recent_events <- c()

    for (i in 1:length(mens_profile_urls$profile_slugs)) {

      player_name <- mens_profile_urls$Name[i]

      message("Scraping ", player_name, "'s recent results")

      Rank <- mens_profile_urls$Rank[i]

      profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

      recent_result <- read_html(profile_url) %>%
                          html_nodes("table")

      recent_result <- suppressWarnings(recent_result[str_detect(recent_result, "Seeding")][[1]]) %>%
                            html_table() %>%
                            as_tibble() %>%
                            filter(row_number() != n()) %>%
                            clean_names() %>%
                            mutate(player = player_name,
                                   rank = Rank,
                                   date = ymd(parse_date_time(date, orders = "bY"))) %>%
                            select(rank, player, everything())

      mens_recent_events <- rbind(mens_recent_events, recent_result)

    }



  } else {

    mens_recent_events <- c()

  }


  ## Women's recent results

  if (length(womens_profile_urls$profile_slugs) > 0) {

    womens_recent_matches <- c()

    for (i in 1:length(womens_profile_urls$profile_slugs)) {

      player_name <- womens_profile_urls$Name[i]

      message("Scraping ", player_name, "'s recent results")

      Rank <- womens_profile_urls$Rank[i]

      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

      recent_result <- read_html(profile_url) %>%
                          html_nodes("table")

      recent_result <- suppressWarnings(recent_result[str_detect(recent_result, "Seeding")][[1]]) %>%
                          html_table() %>%
                          as_tibble() %>%
                          filter(row_number() != n()) %>%
                          clean_names() %>%
                          mutate(player = player_name,
                                 rank = Rank,
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>%
                          select(rank, player, everything())

      womens_recent_events <- rbind(womens_recent_events, recent_result)

    }




  } else {

    womens_recent_events <- c()

  }

  recent_events <- bind_rows(mens_recent_events, womens_recent_events) %>%
                        rename(country = ctry, event_date = date) %>%
                        mutate(seeding = str_extract(seeding, pattern = "[0-9]+"),
                               tour = if_else(tour == "-", NA_character_, tour),
                               round_reached = if_else(round_reached == "-", NA_character_, round_reached))


  recent_events <- recent_events %>%
                        select(rank, player, seeding, round_reached, event, event_date, country, tour)


  return(recent_events)

}
