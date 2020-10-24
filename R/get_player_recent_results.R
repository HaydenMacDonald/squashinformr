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
#' @importFrom plyr round_any
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

    ## If rank is null, filter by name
    if (is.null(rank) == TRUE) {

      mens_profile_urls <- mens_ranking_table %>%
                              filter(str_detect(Name, player))

    } else if (is.null(player) == TRUE) { ## If player is null, filter by rank

      mens_profile_urls <- mens_ranking_table %>%
                              filter(Rank %in% rank)
    }


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

    ## If rank is null, filter by name
    if (is.null(rank) == TRUE) {

      womens_profile_urls <- womens_ranking_table %>%
                                filter(str_detect(Name, player))

    } else if (is.null(player) == TRUE) { ## If player is null, filter by rank

      womens_profile_urls <- womens_ranking_table %>%
                                filter(Rank %in% rank)
    }



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







## Recent Results

  ## Men's recent results

  if (length(mens_profile_urls$profile_slugs) > 0) {

    mens_recent_events <- c()

    for (i in seq_along(mens_profile_urls$profile_slugs)) {

      player_name <- mens_profile_urls$Name[i]

      message("Scraping ", player_name, "'s recent results")

      Rank <- mens_profile_urls$Rank[i]

      profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

      current_page <- suppressMessages(bow(profile_url)) %>%
                                          scrape()

      recent_result <- current_page %>%
                          html_nodes("table")

      recent_result <- suppressWarnings(recent_result[str_detect(recent_result, "Round Reached")][[1]]) %>%
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

    womens_recent_events <- c()

    for (i in seq_along(womens_profile_urls$profile_slugs)) {

      player_name <- womens_profile_urls$Name[i]

      message("Scraping ", player_name, "'s recent results")

      Rank <- womens_profile_urls$Rank[i]

      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

      current_page <- suppressMessages(bow(profile_url)) %>%
                                          scrape()

      recent_result <- current_page %>%
                          html_nodes("table")

      recent_result <- suppressWarnings(recent_result[str_detect(recent_result, "Round Reached")][[1]]) %>%
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
