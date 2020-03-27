#' Get recent PSA rankings from SquashInfo
#'
#' Given a competition category, \code{get_player_rankings_history()} returns the most recent PSA rankings table.
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
#' ## Get the rankings history for the top three women's singles players
#' top_two <- get_player_rankings_history(rank = 1:2, category = "mens")
#'
#' ggplot(top_two) +
#'  geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
#'  scale_y_reverse()
#'
#' ## Get the rankings history for the top three women's singles players
#' top_three <- get_player_rankings_history(rank = 1:3, category = "womens")
#'
#' ggplot(top_three) +
#'   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
#'   scale_y_reverse()
#'
#' ## Get the top 50 players in both men's and women's singles competitions
#' best <- get_player_rankings_history(rank = 1, category = "both")
#'
#' ggplot(best) +
#'   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
#'   scale_y_reverse()
#'
#' @note This function only returns the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
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

get_player_rankings_history <- function(player = NULL, rank = NULL, category = NULL) {

  stopifnot(is.character(player) | is.null(player), nchar(player) > 0, is.numeric(rank) | is.null(rank), is.character(category))

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

    ## Men's profile urls
    mens_profile_urls <- mens_ranking_table %>%
                              filter(Name %in% player | Rank %in% rank) %>%
                              mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

    womens_profile_urls <- c()

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)


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

    ## Women's profile urls
    womens_profile_urls <- womens_ranking_table %>%
                                    filter(Name %in% player | Rank %in% rank) %>%
                                    mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

    mens_profile_urls <- c()

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)



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

    ## Men's profile urls
    mens_profile_urls <- mens_ranking_table %>%
                                  filter(Name %in% player | Rank %in% rank) %>%
                                  mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

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

    ## Womens profile urls
    womens_profile_urls <- womens_ranking_table %>%
                                    filter(Name %in% player | Rank %in% rank) %>%
                                    mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


## Rankings History

  rankings_history <- c()

  for (i in 1:length(all_profile_urls$profile_slugs)) {

    player_name <- all_profile_urls$Name[i]

    current_rank <- all_profile_urls$Rank[i]

    rankings_history_url <- sprintf("http://www.squashinfo.com%s", all_profile_urls$profile_slugs[i])

    result <- read_html(rankings_history_url) %>%
                  html_nodes("table")

    result <- suppressWarnings(result[str_detect(result, "Year")][[1]]) %>%
                  html_table() %>%
                  as_tibble() %>%
                  gather(key = month, value = rank, -Year) %>%
                  rename(year = Year) %>%
                  mutate(name = player_name,
                         current_rank = current_rank,
                         rank = if_else(rank == "-", NA_character_, rank),
                         rank = as.numeric(rank),
                         month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                         exact_date = paste(month, year),
                         exact_date = ymd(parse_date_time(exact_date, orders = "bY"))) %>%
                  select(year, month, exact_date, rank, name, current_rank) %>%
                  arrange(year, month)

    rankings_history <- bind_rows(rankings_history, result)

  }

  return(rankings_history)

}



