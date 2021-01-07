#' Get tournaments from SquashInfo
#'
#' Given a year, \code{get_tournaments()} returns data for PSA World Tour tournaments and other events.
#'
#' @param year integer indicating the tournament year. Must be one of 2020 or 2021.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the league, competition category, name, date, city, and country.
#'
#' @examples
#'
#' ## Get data on 2021 PSA World Tour tournaments
#' \donttest{get_tournaments()}
#'
#' ## Get data on 2020 non-PSA World Tour tournaments
#' \donttest{get_tournaments(2020, world_tour = FALSE)}
#'
#' @note This function only returns tournaments from 2020 and 2021, as any other data are not available to non-premium members on SquashInfo.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/results}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr desc
#' @importFrom dplyr arrange
#' @importFrom polite bow
#' @importFrom polite nod
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr regex
#' @importFrom stringr str_trim
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom stringr str_count
#' @importFrom lubridate year
#' @importFrom lubridate ymd
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#'
#' @export

get_tournaments <- function(year = 2021, world_tour = TRUE) {

    ## Input errors
    stopifnot(is.numeric(year) | is.null(year), (nchar(trunc(abs(year))) == 4 & year > 0) | is.null(year), is.logical(world_tour))

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

      ## Filter out non-world tour tournaments
      if (world_tour == TRUE) {
        results <- results %>%
          filter(str_detect(league, pattern = "World"))

      }

      ## Bind results row-wise
      tournaments <- rbind(tournaments, results)

      ## If results begin to include previous year tournaments, break while loop, otherwise continue to next page
      if ((year - 1) %in% year(tournaments$date)) {
        results_limit <- NA_character_

      } else {
        results_limit <- 0

        # Find url in "Next" button
        t_url <- current_page %>%
          html_nodes("a")

        t_url <-
          suppressWarnings(t_url[str_detect(t_url, "Next")]) %>%
          html_attr("href")

        ## Replace t_url with url in Next page button
        t_url <- sprintf("http://www.squashinfo.com%s", t_url)

      }

    }


    ## Clean tournament results
    tournaments <- tournaments %>%
      as_tibble() %>%
      select(league, name, location, date) %>%
      mutate(
        league = if_else(league == "", NA_character_, league),
        ## Replace empty strings with NAs
        category = if_else(
          str_detect(name, regex("\\(M\\)")),
          "Men's",
          if_else(str_detect(name, regex("\\(W\\)")), "Women's", NA_character_)
        ),
        ## Derive category from tournament name
        name = str_replace_all(name, pattern = regex(" \\(M\\)"), ""),
        name = str_replace_all(name, pattern = regex(" \\(W\\)"), ""),
        city = str_extract(location, "(.*)(?=, )"),
        ## extract city and country from location
        country = if_else(
          str_count(location, ",") == 2,
          str_trim(str_remove(location, "([^,]+,[^,]+),."), side = "left"),
          str_trim(str_extract(location, "(?<=, )(.*)"), side = "left")
        )
      ) %>%
      filter(date >= ymd('2020-01-01')) %>%  ## filter out any tournaments occuring before 2020-01-01 (tournament data before this date is not available to regular members of SquashInfo)
      arrange(desc(date)) %>%
      select(league, category, name, date, city, country)


    ## Filter final results by year input
    if (year == 2021) {
      tournaments <- tournaments %>%
        filter(year(date) == 2021)


    } else if (year == 2020) {
      tournaments <- tournaments %>%
        filter(year(date) == 2020)

    }


    return(tournaments)

  }
