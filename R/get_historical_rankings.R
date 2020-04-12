#' Get historical PSA rankings from SquashInfo
#'
#' Given a year, abbreviated month, competition category, and number of top players, \code{get_historical_rankings()} returns the corresponding historical PSA rankings table.
#'
#' @param year integer indicating the ranking year
#'
#' @param month three letter abbreviation of ranking month
#'
#' @param category character string indicating the competition category. Must be one of "both", "mens", or "womens".
#'
#' @param top integer indicating the number of top PSA players by rank to return.
#'
#' @return Tibble containing the ranking year, ranking month, exact date, rank, player name, and player current rank.
#'
#' @examples
#'
#' ## Who were the top 20 ranked men's singles players in December 2015?
#' \donttest{top20 <- get_historical_rankings(2015, "Dec", "mens", 20)}
#'
#' ## Who were the top 50 ranked women's singles players in January 2018?
#' \donttest{top50 <- get_historical_rankings(2018, "Jan", "womens", 50)}
#'
#' ## Who were the top 10 players in both men's and women's singles in January 2019?
#' \donttest{top10 <- get_historical_rankings(2019, "Jan", "both", 10)}
#'
#' @note This function takes substantial time to scrape the corresponding data because historical ranking tables are not available to non-premium members on SquashInfo. The historical rankings returned by this function are reverse-engineered from individual player ranking histories.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/players}
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
#' @importFrom polite bow
#' @importFrom polite nod
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#' @importFrom rlang is_empty
#'
#' @export

get_historical_rankings <- function(year = NULL, month = NULL, category = NULL, top = NULL) {

  stopifnot(is.numeric(year), is.character(month), nchar(month) == 3, is.numeric(top) | is.null(top), is.character(category))

  category <- tolower(category)

  month <- paste(toupper(substr(month, 1, 1)), tolower(substr(month, 2, nchar(month))), sep = "")

  if (category == "mens") {

  # Men profile slugs

    ## Get player profile URLs for men
    players_url <- "http://www.squashinfo.com/players?start=1"

    ## Create mens_profile_urls
    mens_player_table <- c()

    while(!is.na(players_url)){

      # Verbose
      message("Scraping ", players_url)

      # Check players_url for Robots.txt
      suppressMessages(session <- bow(players_url))

      # Nod and scrape politely
      current_page <- nod(session, players_url) %>%
                                       scrape(verbose = TRUE)

      # Extract player profile urls
      profile_url <- current_page %>%
                        html_nodes("table")

      # Clean player profile urls
      profile_url <- suppressWarnings(profile_url[str_detect(profile_url, "Rank")][[1]]) %>%
                        html_nodes("a") %>%
                        html_nodes(xpath = "//*[@class='event_player']") %>%
                        html_attr("href")

      # Extract player table
      result <- current_page %>%
                    html_nodes("table")

      # Clean player table
      result <- suppressWarnings(result[str_detect(result, "Rank")][[1]]) %>%
                    html_table() %>%
                    clean_names() %>%
                    as_tibble() %>%
                    rename(gender = g) %>%
                    select(rank, hwr, name, gender) %>%
                    mutate(rank = if_else(rank == "-", NA_character_, rank),
                           rank = as.numeric(rank),
                           hwr = if_else(hwr == "-", NA_character_, hwr),
                           hwr = as.numeric(hwr),
                           profile_url = profile_url) %>%
                    filter(gender == "M", hwr <= if_else(is.null(top) == TRUE, 500, top))

      # Add to results to mens_player_table
      mens_player_table <- rbind(mens_player_table, result)

      # Check if there's a "Next" button
      players_url <- current_page %>%
                          html_nodes("a")

      players_url <- suppressWarnings(players_url[str_detect(players_url, "Next")])

      if (is_empty(players_url) == TRUE) {

        players_url <- NA_character_

      } else {

        ## When there is a "Next" button, replace players_url with new Next page url
        players_url <- players_url[[1]] %>%
                              html_attr("href")

        players_url <- sprintf("http://www.squashinfo.com%s", players_url)

      }


    } # end while loop

    ## Men's profile urls
    mens_profile_urls <- mens_player_table %>%
                              mutate(profile_url = str_replace(profile_url, pattern = "players", replacement = "rankings"))

    womens_profile_urls <- c()

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)


  } else if (category == "womens") {

  # Women profile slugs

    ## Get player profile URLs for men
    players_url <- "http://www.squashinfo.com/players?start=1"

    ## Create mens_profile_urls
    womens_player_table <- c()

    while(!is.na(players_url)){

      # Verbose
      message("Scraping ", players_url)

      # Check players_url for Robots.txt
      suppressMessages(session <- bow(players_url))

      # Nod and scrape politely
      current_page <- nod(session, players_url) %>%
                                      scrape(verbose = TRUE)

      # Extract player profile urls
      profile_url <- current_page %>%
                          html_nodes("table")

      # Clean player profile urls
      profile_url <- suppressWarnings(profile_url[str_detect(profile_url, "Rank")][[1]]) %>%
                        html_nodes("a") %>%
                        html_nodes(xpath = "//*[@class='event_player']") %>%
                        html_attr("href")

      # Extract player table
      result <- current_page %>%
                    html_nodes("table")

      # Clean player table
      result <- suppressWarnings(result[str_detect(result, "Rank")][[1]]) %>%
                    html_table() %>%
                    clean_names() %>%
                    as_tibble() %>%
                    rename(gender = g) %>%
                    select(rank, hwr, name, gender) %>%
                    mutate(rank = if_else(rank == "-", NA_character_, rank),
                           rank = as.numeric(rank),
                           hwr = if_else(hwr == "-", NA_character_, hwr),
                           hwr = as.numeric(hwr),
                           profile_url = profile_url) %>%
                    filter(gender == "F", hwr <= if_else(is.null(top) == TRUE, 500, top))

      # Add to results to mens_player_table
      womens_player_table <- rbind(womens_player_table, result)

      # Check if there's a "Next" button
      players_url <- current_page %>%
                        html_nodes("a")

      players_url <- suppressWarnings(players_url[str_detect(players_url, "Next")])

      if (is_empty(players_url) == TRUE) {

        players_url <- NA_character_

      } else {

        ## When there is a "Next" button, replace players_url with new Next page url
        players_url <- players_url[[1]] %>%
                                html_attr("href")

        players_url <- sprintf("http://www.squashinfo.com%s", players_url)

      }


    } # end while loop

    ## Women's profile urls
    womens_profile_urls <- womens_player_table %>%
                                mutate(profile_url = str_replace(profile_url, pattern = "players", replacement = "rankings"))

    mens_profile_urls <- c()

    ## Combine men and women's profile urls
    all_profile_urls <- bind_rows(mens_profile_urls, womens_profile_urls)



  } else if (category == "both" | is.null(category) == TRUE) {

  ## All profile slugs

    ## Get player profile URLs for both men and women
    players_url <- "http://www.squashinfo.com/players?start=1"

    ## Create mens_profile_urls
    full_player_table <- c()

    while(!is.na(players_url)){

      # Verbose
      message("Scraping ", players_url)

      # Check players_url for Robots.txt
      suppressMessages(session <- bow(players_url))

      # Nod and scrape politely
      current_page <- nod(session, players_url) %>%
                                        scrape(verbose = TRUE)

      # Extract player profile urls
      profile_url <- current_page %>%
                        html_nodes("table")

      # Clean player profile urls
      profile_url <- suppressWarnings(profile_url[str_detect(profile_url, "Rank")][[1]]) %>%
                        html_nodes("a") %>%
                        html_nodes(xpath = "//*[@class='event_player']") %>%
                        html_attr("href")

      # Extract player table
      result <- current_page %>%
                    html_nodes("table")

      # Clean player table
      result <- suppressWarnings(result[str_detect(result, "Rank")][[1]]) %>%
                    html_table() %>%
                    clean_names() %>%
                    as_tibble() %>%
                    rename(gender = g) %>%
                    select(rank, hwr, name, gender) %>%
                    mutate(rank = if_else(rank == "-", NA_character_, rank),
                           rank = as.numeric(rank),
                           hwr = if_else(hwr == "-", NA_character_, hwr),
                           hwr = as.numeric(hwr),
                           profile_url = profile_url) %>%
                    filter(hwr <= if_else(is.null(top) == TRUE, 500, top))

      # Add to results to mens_player_table
      full_player_table <- rbind(full_player_table, result)

      # Check if there's a "Next" button
      players_url <- current_page %>%
                          html_nodes("a")

      players_url <- suppressWarnings(players_url[str_detect(players_url, "Next")])

      if (is_empty(players_url) == TRUE) {

        players_url <- NA_character_

      } else {

        ## When there is a "Next" button, replace players_url with new Next page url
        players_url <- players_url[[1]] %>%
                                html_attr("href")

        players_url <- sprintf("http://www.squashinfo.com%s", players_url)

      }


    } # end while loop

    ## Men's profile urls
    all_profile_urls <- full_player_table %>%
                                mutate(profile_url = str_replace(profile_url, pattern = "players", replacement = "rankings"))


  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


## Rankings History

  rankings_history <- c()

  for (i in 1:length(all_profile_urls$profile_url)) {

    ## Extract player name
    player_name <- all_profile_urls$name[i]

    ## Verbose
    message("Scraping ", player_name, "'s rankings history")

    ## Current rank
    current_rank <- all_profile_urls$rank[i]

    ## Highest rank achieved
    highest_rank <- all_profile_urls$hwr[i]

    ## Ranking table url
    rankings_history_url <- sprintf("http://www.squashinfo.com%s", all_profile_urls$profile_url[i])

    ## Bow and scrape page
    result <- suppressMessages(bow(rankings_history_url) %>%
                                      scrape(verbose = TRUE))
    ## Find html tables
    result <- result %>%
                html_nodes("table")

    ## Find ranking history table and clean results
    result <- suppressWarnings(result[str_detect(result, "Year")][[1]]) %>%
                  html_table() %>%
                  as_tibble() %>%
                  ## Convert month columns to character
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
                  pivot_longer(-Year, names_to = "ranking_month", values_to = "rank") %>%  ## transform data to long format
                  rename(ranking_year = Year) %>%
                  filter(ranking_year == year, ranking_month == month) %>% ## Filter results according to year and month input from user
                  rename(year = ranking_year, month = ranking_month) %>%
                  ## Added data extracted earlier
                  mutate(name = player_name,
                         current_rank = current_rank,
                         highest_rank = highest_rank,
                         rank = suppressWarnings(as.numeric(rank)),
                         month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), ## make month an ordered factor
                         exact_date = paste(month, year),
                         exact_date = ymd(parse_date_time(exact_date, orders = "bY"))) %>% ## convert date to yyyy-mm-dd
                  filter(rank <= if_else(is.null(top) == TRUE, 500, top)) %>%  ## filter out observations with rank >= 500
                  select(year, month, exact_date, rank, name, current_rank) %>%
                  arrange(year, month)

    ## Bind results row-wise
    rankings_history <- rbind(rankings_history, result)

  }

  ## Order results by ascending rank
  rankings <- rankings_history %>%
                          arrange(rank)

  return(rankings)


}
