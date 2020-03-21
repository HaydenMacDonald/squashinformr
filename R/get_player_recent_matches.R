#' Get a player's recent matches from SquashInfo
#'
#' Returns scraped recent match data for men and/or women players
#'
#'
#' @param player character string of the player's name
#'
#'
#' @return Tibble containing first name, last name, age, gender, birthplace, nationality, residence, height in cm, weight in kg, plays (handedness), racket brand, year of joining PSA, university, and club.
#'
#' @examples
#'
#' get_players(top = 25, category = "women")
#'
#' both <- get_players(5, "both")
#'
#' @note This function only returns players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men}
#'     \url{http://www.squashinfo.com/rankings/women}
#'
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

get_player_recent_matches <- function(player = NULL, rank = NULL, category = c("both", "mens", "womens"), from = NULL, to = NULL) {

  stopifnot(is.character(player), nchar(player) > 0, is.numeric(rank), is.Date(from), is.Date(to))

  if (is.null(player) == TRUE & is.null(rank) == TRUE) {

    stop("Either a player's full name or ")

  }


  if (category == "men") {

# Men profile slugs

    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    mens_ranking_table <- c()

    for (i in 1:10) {

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
                                filter(Name %in% player | Rank == rank)



  } else if (category == "women") {

# Women slugs

    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url, verbose = FALSE))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    for (i in 1:8) {

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
                                    filter(Name %in% player | Rank == rank)



  } else if (category == "both" | is.null(category) == TRUE) {

        # Men profile slugs

        ## Get profile URLs for top n men
        rankings_url <- "http://www.squashinfo.com/rankings/men"

        ## Check URL for Robots.txt
        suppressMessages(session <- bow(rankings_url))

        ## Create mens_profile_urls
        mens_ranking_table <- c()

        for (i in 1:10) {

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
                                    filter(Name %in% player | Rank == rank)

        # Women slugs

        ## Get profile URLs for top n women
        rankings_url <- "http://www.squashinfo.com/rankings/women"

        ## Check URL for Robots.txt
        suppressMessages(session <- bow(rankings_url, verbose = FALSE))

        ## Create mens_profile_urls
        womens_ranking_table <- c()

        for (i in 1:8) {

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
                                          filter(Name %in% player | Rank == rank)

  } else {

    cat("\n")

    stop("category must be one of 'both', 'mens', or 'womens'")


}







## Recent Matches

  recent_matches <- c()

  ## Men's results

  if (length(mens_profile_urls$profile_slugs) > 0) {

      mens_recent_matches <- c()

      for (i in 1:length(mens_profile_urls$profile_slugs)) {

        player_name <- mens_profile_urls$Name[i]

        profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls$profile_slugs[i])

        recent_result <- read_html(profile_url) %>%
                              html_nodes("table") %>%
                              .[[4]] %>%
                              html_table() %>%
                              filter(row_number() != n()) %>%
                              as_tibble() %>%
                              clean_names() %>%
                              mutate(player = player_name) %>%
                              select(player, everything())

      }

      mens_recent_matches <- bind_rows(mens_recent_matches, recent_result)

  } else {

    mens_recent_matches <- c()

  }


  ## Women's results

  if (length(womens_profile_urls$profile_slugs) > 0) {

      womens_recent_matches <- c()

      for (i in 1:length(womens_profile_urls$profile_slugs)) {

        player_name <- womens_profile_urls$Name[i]

        profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls$profile_slugs[i])

        recent_result <- read_html(profile_url) %>%
                              html_nodes("table") %>%
                              .[[4]] %>%
                              html_table() %>%
                              filter(row_number() != n()) %>%
                              as_tibble() %>%
                              clean_names() %>%
                              mutate(player = player_name) %>%
                              select(player, everything())
      }

      womens_recent_matches <- bind_rows(womens_recent_matches, recent_result)


  } else {

    womens_recent_matches <- c()

  }

  recent_matches <- bind_rows(mens_recent_matches, womens_recent_matches) %>%
                          rename(round = rnd, country = ctry, result = w_l) %>%
                          mutate(round = toupper(round),
                                 match_time = if_else(opponent == "bye", NA_character_, str_extract(score, pattern = regex("[:digit:]{2,}m"))),
                                 match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                                 date = parse_date_time(date, orders = "bY")) %>%
                          select(player, opponent, result, match_time, date, round, event, psa, country)

  return(recent_matches)

}









