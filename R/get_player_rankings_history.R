#' Get a player's PSA ranking history from SquashInfo
#'
#' Given a player name or rank, and a competition category, \code{get_player_rankings_history()} returns a tidy version of the PSA rankings history table for that player(s).
#'
#' @param player character string of player name(s).
#'
#' @param rank integer indicating the rank of the PSA player(s) to return.
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#'
#' @return Tibble containing the year, month, exact date, rank, player name and player's current rank.
#'
#' @examples
#'
#' ## Get the rankings history for the top two men's singles players
#' top_two <- get_player_rankings_history(rank = 1:2, category = "mens")
#'
#'
#' ## Get the rankings history for the top three women's singles players
#' top_three <- get_player_rankings_history(rank = 1:3, category = "womens")
#'
#'
#'
#' @note This function only returns PSA ranking histories for players currently ranked in PSA Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom plyr round_any
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom tibble as_tibble
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
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_profile_urls
    womens_ranking_table <- c()

    for (i in if (is.null(rank) == TRUE) {1:8} else {1:(round_any(max(rank), 50, ceiling)/50)}) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)

      ## Scrape table for player profile hrefs

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

    ## Player's name
    player_name <- all_profile_urls$Name[i]

    ## Verbose
    message("Scraping ", player_name, "'s ranking history")

    ## Player's rank
    current_rank <- all_profile_urls$Rank[i]

    ## Ranking history table url
    rankings_history_url <- sprintf("http://www.squashinfo.com%s", all_profile_urls$profile_slugs[i])

    ## Bow and scrape
    current_page <- suppressMessages(bow(rankings_history_url)) %>%
                                        scrape()
    ## Find html tables
    result <- current_page %>%
                  html_nodes("table")

    ## Extract the rankings history table and clean results
    result <- suppressWarnings(result[str_detect(result, "Year")][[1]]) %>%
                  html_table() %>%
                  as_tibble() %>%
                  ## Make month columns character
                  mutate(Jan = as.character(Jan),
                         Feb = as.character(Feb),
                         Mar = as.character(Mar),
                         Apr = as.character(Apr),
                         May = as.character(May),
                         Jun = as.character(Jun),
                         Jul = as.character(Jul),
                         Aug = as.character(Aug),
                         Sep = as.character(Sep),
                         Oct = as.character(Oct),
                         Nov = as.character(Nov),
                         Dec = as.character(Dec)) %>%
                  pivot_longer(-Year, names_to = "month", values_to = "rank") %>%
                  rename(year = Year) %>%
                  mutate(name = player_name,
                         current_rank = current_rank,
                         rank = if_else(rank == "-", NA_character_, rank), #replace hypens with NAs
                         rank = as.numeric(rank), ## make rank numeric
                         month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), ## make month an ordered factor
                         ## Create an exact date for each ranking (yyyy-mm-dd)
                         exact_date = paste(month, year),
                         exact_date = ymd(parse_date_time(exact_date, orders = "bY"))) %>%
                  select(year, month, exact_date, rank, name, current_rank) %>%
                  arrange(year, month)

    ## Bind results row-wise
    rankings_history <- bind_rows(rankings_history, result)

  }

  return(rankings_history)

}



