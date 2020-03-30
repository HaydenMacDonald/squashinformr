#' Get tournaments from SquashInfo
#'
#' Given a year, \code{get_tournaments()} returns data for PSA World Tour tournaments and other events.
#'
#' @param year integer indicating the tournament year. Must be one of 2019 or 2020.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the league, competition category, name, date, city, and country.
#'
#' @examples
#'
#' ## Get data on 2020 PSA World Tour tournaments
#' get_tournaments()
#'
#' ## Get data on 2019 non-PSA World Tour tournaments
#' get_tournaments(2019, world_tour = FALSE)
#'
#' @note This function only returns tournaments from 2019 and 2020, as any other data are not available to non-premium members on SquashInfo.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/results}
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

get_tournaments <- function(year = 2020, world_tour = FALSE) {

  stopifnot(is.numeric(year) | is.null(year), is.logical(world_tour))

  # Results page on SquashInfo
  t_url <- "http://www.squashinfo.com/results?start=1"

  # Empty tournaments object
  tournaments <- c()

  # Set arbitrary variable to test for tournament result limit
  results_limit <- 0

  while (!is.na(results_limit)) {

    ## Verbose
    message("Scraping ", t_url)

    # Check for Robots.txt
    session <- suppressMessages(bow(t_url))

    # Nod and scrape page politely
    current_page <- nod(session, t_url) %>%
                          scrape(verbose = FALSE)

    ## Extract tournaments table
    results <- current_page %>%
                  html_nodes("div.darkborder") %>%
                  html_nodes("table") %>%
                  .[[2]] %>%
                  html_table(header = TRUE) %>%
                  as.data.frame() %>%
                  clean_names() %>%
                  rename(league = x) %>%
                  mutate(date = dmy(date)) %>%
                  filter(!str_detect(name, pattern = "Premier League"))

    if (world_tour == TRUE) {

      results <- results %>%
                    filter(str_detect(league, pattern = "World"))

    }

    tournaments <- rbind(tournaments, results)

    if (2018 %in% year(tournaments$date)) {

      results_limit <- NA_character_

    } else {

      results_limit <- 0

      # Find url in "Next" button
      t_url <- current_page %>%
                          html_nodes("a")

      t_url <- suppressWarnings(t_url[str_detect(t_url, "Next")]) %>%
                  html_attr("href")

      ## Replace t_url with url in Next page button
      t_url <- sprintf("http://www.squashinfo.com%s", t_url)

    }

  }


  tournaments <- tournaments %>%
                    as_tibble() %>%
                    select(league, name, location, date) %>%
                    mutate(league = if_else(league == "", NA_character_, league),
                           category = if_else(str_detect(name, regex("\\(M\\)")), "Men's", if_else(str_detect(name, regex("\\(W\\)")), "Women's", NA_character_)),
                           name = str_replace_all(name, pattern = regex(" \\(M\\)"), ""),
                           name = str_replace_all(name, pattern = regex(" \\(W\\)"), ""),
                           city = str_replace_all(gsub(" .*","\\1", location), ",", replacement = ""),
                           country = gsub(".* ","\\2", location)) %>%
                    filter(date >= ymd('2019-01-01')) %>%
                    arrange(desc(date)) %>%
                    select(league, category, name, date, city, country)


  if (year == 2020) {

    tournaments <- tournaments %>%
                        filter(year(date) == 2020)


  } else if (year == 2019) {

    tournaments <- tournaments %>%
                        filter(year(date) == 2019)

  }


  return(tournaments)

}
