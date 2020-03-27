#' Get recent PSA rankings from SquashInfo
#'
#' Given a competition category, \code{get_rankings()} returns the most recent PSA rankings table.
#'
#'
#' @param top integer indicating the number of top PSA players by rank to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the player rank, previous month's rank, name, highest ranking achieved, date of highest ranking, nationality, and competition category.
#'
#' @examples
#'
#' ## Get the top 25 ranked men's singles players
#' get_rankings(top = 25, category = "men")
#'
#' ## Get the top 100 ranked women's singles players
#' get_rankings(top = 100, category = "womens")
#'
#' ## Get the top 50 players in both men's and women's singles competitions
#' get_rankings(top = 50, category = "both")
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

get_rankings2 <- function(year = NULL, month = NULL, top = NULL, category = NULL) {

  stopifnot(is.numeric(year), is.character(month), nchar(month) == 3, is.numeric(top) | is.null(top), is.character(category))

  category <- tolower(category)

  month <- paste(toupper(substr(month, 1, 1)), substr(month, 2, nchar(month)), sep = "")

  if (category == "mens") {

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

    ## Men's profile urls
    mens_profile_urls <- mens_ranking_table %>%
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

    ## Women's profile urls
    womens_profile_urls <- womens_ranking_table %>%
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

    ## Men's profile urls
    mens_profile_urls <- mens_ranking_table %>%
                                  mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

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

    ## Womens profile urls
    womens_profile_urls <- womens_ranking_table %>%
                                    mutate(profile_slugs = str_replace(profile_slugs, pattern = "players", replacement = "rankings"))

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


  ## Rankings

  rankings <- c()

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
                  filter(year == year, month == month) %>%
                  mutate(name = player_name,
                         current_rank = current_rank,
                         rank = if_else(rank == "-", NA_character_, rank),
                         rank = as.numeric(rank),
                         exact_date = paste(month, year),
                         exact_date = ymd(parse_date_time(exact_date, orders = "bY"))) %>%
                  select(year, month, exact_date, rank, name, current_rank)

    rankings <- bind_rows(rankings, result)

  }

  rankings <- rankings %>%
                  arrange(rank)

  return(rankings)

}
