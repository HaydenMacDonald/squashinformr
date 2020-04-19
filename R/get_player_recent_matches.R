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
#' get_player_recent_matches(player = "Mohamed Elshorbagy", category = "mens")
#'
#' ## Get Nour El Tayeb's recent match data
#' get_player_recent_matches("El Tayeb", category = "womens")
#'
#' ## Get recent match data from the top two players in both Men's and Women's competitions
#' get_player_recent_matches(rank = 1:2, category = "both")
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
#' @importFrom stringr str_extract_all
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom Hmisc %nin%
#' @importFrom utils tail
#'
#' @export

get_player_recent_matches <- function(player = NULL, rank = NULL, category = NULL) {

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



  } else if (category == "both") {

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

      ## Create empty mens_recent_matches dataframe
      mens_recent_matches <- c()

      for (i in 1:length(mens_profile_urls$profile_slugs)) { ## For every men's profile slug

        ## Extract player name
        player_name <- mens_profile_urls$Name[i]

        ## Extract current rank
        Rank <- mens_profile_urls$Rank[i]

        ## Create profile url from slug
        profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

        ## Verbose
        message("Scraping ", player_name, "'s profile")

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
        mens_recent_matches <- rbind(mens_recent_matches, recent_result)

      }



  } else { ## If there are 0 men's profile slugs

    ## Create empty mens_recent_matches dataframe
    mens_recent_matches <- c()

  }


  ## Women's results

  if (length(womens_profile_urls$profile_slugs) > 0) { ## If there are more than 0 women's profile slugs

      ## Create empty womens_recent_matches dataframe
      womens_recent_matches <- c()

      for (i in 1:length(womens_profile_urls$profile_slugs)) { ## For every women's profile slug

        ## Extract player name
        player_name <- womens_profile_urls$Name[i]

        ## Extract current rank
        Rank <- womens_profile_urls$Rank[i]

        ## Create profile url from slug
        profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

        ## Verbose
        message("Scraping ", player_name, "'s profile")

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
                                     date = ymd(parse_date_time(date, orders = "bY"))) %>%
                              select(rank, player, everything())

        ## Bind results row-wise
        womens_recent_matches <- rbind(womens_recent_matches, recent_result)

      }




  } else { ## If there are 0 women's profile slugs

    ## Create empty womens_recent_matches dataframe
    womens_recent_matches <- c()

  }

  ## Bind results row-wise
  recent_matches <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
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
  for (j in 1:length(recent_matches$score)) {

    ## If there are no games, next row, create match object
    if (length(recent_matches$score[[j]]) == 0) { next } else { match <- recent_matches$score[[j]] }

    ## Start with 0 games won and games lost
    wins <- 0

    losses <- 0

    ## For each game in each match
    for (i in 1:length(match)) {

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

}









