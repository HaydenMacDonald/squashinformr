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
#' get_player_recent_games(player = "Mohamed Elshorbagy", category = "mens")
#'
#' ## Get Nour El Tayeb's recent game data
#' get_player_recent_games("El Tayeb", category = "womens")
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
#' @importFrom tidyr unnest
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
#' @importFrom utils tail
#'
#' @export


get_player_recent_games <- function(player = NULL, rank = NULL, category = NULL) {

  stopifnot(is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank))

  if (is.null(player) == TRUE & is.null(rank) == TRUE) {

    stop("Either a player's full name or rank is required")

  }

  if (length(player) != 1 & is.null(rank)) {

    stop("A single player's full name is required")

  }


  if (length(rank) != 1 & is.numeric(rank) & is.null(player) == FALSE) {

    stop("Do not provide player names when supplying multiple ranks")

  }


  ## If querying both competition categories, only accept ranks
  if (category == "both") {

    if (is.null(player) == FALSE | is.null(rank) == TRUE) {

      stop("When scraping across competition categories, only provide ranks")

    }

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

    ## Rankings table url
    rankings_url <- "http://www.squashinfo.com/rankings/men/1"

    if (is.null(player) == FALSE & is.null(rank) == TRUE) {

      while(TRUE %nin% str_detect(mens_ranking_table$Name, player)) {

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

    } else if (is.null(player) == TRUE & is.null(rank) == FALSE) {

      while(tail(rank, n = 1) %nin% mens_ranking_table$Rank) {

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
                                  filter(if (is.null(rank)) {str_detect(Name, player)} else if (is.null(player)) {Rank %in% rank})

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

    if (is.null(player) == FALSE & is.null(rank) == TRUE) {

      while(TRUE %nin% str_detect(womens_ranking_table$Name, player)) {

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

    } else if (is.null(player) == TRUE & is.null(rank) == FALSE) {

      while(tail(rank, n = 1) %nin% womens_ranking_table$Rank) {

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
                                    filter(if (is.null(rank)) {str_detect(Name, player)} else if (is.null(player)) {Rank %in% rank})

    mens_profile_urls <- c()



  } else if (category == "both" | is.null(category) == TRUE) {

    # Men profile slugs

    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    mens_ranking_table <- c()

    ## Rankings table url
    rankings_url <- "http://www.squashinfo.com/rankings/men/1"

    if (is.null(player) == TRUE & is.null(rank) == FALSE) {

      while(tail(rank, n = 1) %nin% mens_ranking_table$Rank) {

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
                                  filter(Rank %in% rank)

    # Women slugs

    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url, verbose = FALSE))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    ## Rankings table url
    rankings_url <- "http://www.squashinfo.com/rankings/women/1"

    if (is.null(player) == TRUE & is.null(rank) == FALSE) {

      while(tail(rank, n = 1) %nin% womens_ranking_table$Rank) {

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
                                    filter(Rank %in% rank)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")


  }







  ## Recent Matches

  ## Men's results

  if (length(mens_profile_urls$profile_slugs) > 0) { ## If there are more than 0 men's profile slugs

    ## Create mens_recent_matches dataframe
    mens_recent_matches <- c()

    ## For every men's profile slug
    for (i in 1:length(mens_profile_urls$profile_slugs)) {

      ## Extract player name
      player_name <- mens_profile_urls$Name[i]

      ## Extract current rank
      Rank <- mens_profile_urls$Rank[i]

      ## Create porofile url from slug
      profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

      ## Bow and scrape page
      current_page <- suppressMessages(bow(profile_url)) %>%
                                          scrape()

      ## Find html table
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

      ## Bind results row-wise
      mens_recent_matches <- rbind(mens_recent_matches, recent_result)

    }



  } else { ## If there are 0 men's profile slugs

    ## Create mens_recent_matches dataframe
    mens_recent_matches <- c()

  }


  ## Women's results

  if (length(womens_profile_urls$profile_slugs) > 0) { ## If there are more than 0 women's profile slugs

    ## Create womens_recent_matches dataframe
    womens_recent_matches <- c()

    for (i in 1:length(womens_profile_urls$profile_slugs)) { ## for every women's profile slug

      ## Extract player name
      player_name <- womens_profile_urls$Name[i]

      ## Extract current rank
      Rank <- womens_profile_urls$Rank[i]

      ## Create profile url from slug
      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

      ## Bow and scrape page
      current_page <- suppressMessages(bow(profile_url)) %>%
                                          scrape()

      ## Find html table
      recent_result <- current_page %>%
                          html_nodes("table") %>%
                          .[[4]] %>%
                          html_table() %>%
                          filter(row_number() != n()) %>%
                          as_tibble() %>%
                          clean_names() %>%
                          mutate(player = player_name,
                                 rank = Rank,
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>%  ## convert date to yyyy-mm-dd
                          select(rank, player, everything())

      ## Bind results row-wise
      womens_recent_matches <- rbind(womens_recent_matches, recent_result)

    }


  } else { ## If there are 0 women's profile slugs

    ## Create empty womens_recent_matches dataframe
    womens_recent_matches <- c()

  }

  ## Bind results row-wise
  recent_games <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
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
