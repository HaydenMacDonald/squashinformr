#' Get the most recent PSA rankings from SquashInfo
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
#' ## Get the top 10 ranked men's singles players
#' get_rankings(top = 10, category = "mens")
#'
#' ## Get the top 10 ranked women's singles players
#' \donttest{get_rankings(top = 20, category = "womens")}
#'
#' ## Get the top 20 players in both men's and women's singles competitions
#' \donttest{get_rankings(top = 20, category = "both")}
#'
#' @note This function only returns the most recent PSA rankings table for Men's and Women's singles competitions.
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
#' @importFrom dplyr bind_rows
#' @importFrom plyr round_any
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
#' @importFrom lubridate ymd
#' @importFrom lubridate parse_date_time
#'
#' @export

get_rankings <- function(top = NULL, category = NULL) {

  stopifnot(is.numeric(top) | is.null(top), is.character(category))

  category <- tolower(category)

  if (category == "mens") {

    ## Ranking url
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_rankings
    mens_rankings <- c()

    for (i in 1:(round_any(top, 50, ceiling)/50)) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)

      ## Scrape rankings table
      current_page <- suppressMessages(bow(rankings_url)) %>%
                                          scrape()

      results <- current_page %>%
                    html_nodes("table") %>%
                    html_table()

      ## Store results in mens_rankings
      mens_rankings <- bind_rows(mens_rankings, results)

    }

    ## Clean mens_rankings
    mens_rankings <- mens_rankings %>%
                        clean_names() %>%
                        filter(rank <= top) %>%
                        mutate(category = "Men's",
                               date = ymd(parse_date_time(date, orders = "bY"))) %>%
                        rename(previous_rank = prev,
                               highest_world_ranking = hwr,
                               hwr_date = date) %>%
                        select(rank, previous_rank, name, highest_world_ranking, hwr_date, country, category) %>%
                        as_tibble()

    return(mens_rankings)


  } else if (category == "womens") {

    ## Ranking url
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create womens_rankings
    womens_rankings <- c()

    for (i in 1:(round_any(top, 50, ceiling)/50)) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)

      ## Scrape rankings table
      current_page <- suppressMessages(bow(rankings_url)) %>%
                                        scrape()

      results <- current_page %>%
                    html_nodes("table") %>%
                    html_table()

      ## Store results in mens_rankings
      womens_rankings <- bind_rows(womens_rankings, results)

    }

    ## Clean mens_rankings
    womens_rankings <- womens_rankings %>%
                          clean_names() %>%
                          filter(rank <= top) %>%
                          mutate(category = "Women's",
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>%
                          rename(previous_rank = prev,
                                 highest_world_ranking = hwr,
                                 hwr_date = date) %>%
                          select(rank, previous_rank, name, highest_world_ranking, hwr_date, country, category) %>%
                          as_tibble()

    return(womens_rankings)


  } else if (category == "both") {

  ## Men's rankings

    ## Ranking url
    rankings_url <- "http://www.squashinfo.com/rankings/men"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create mens_rankings
    mens_rankings <- c()

    for (i in 1:(round_any(top, 50, ceiling)/50)) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)

      ## Scrape rankings table
      current_page <- suppressMessages(bow(rankings_url)) %>%
                                          scrape()

      results <- current_page %>%
                    html_nodes("table") %>%
                    html_table()

      ## Store results in mens_rankings
      mens_rankings <- bind_rows(mens_rankings, results)

    }

    ## Clean mens_rankings
    mens_rankings <- mens_rankings %>%
                        clean_names() %>%
                        filter(rank <= top) %>%
                        mutate(category = "Men's",
                               date = ymd(parse_date_time(date, orders = "bY"))) %>%
                        rename(previous_rank = prev,
                               highest_ranking = hwr,
                               hwr_date = date) %>%
                        select(rank, previous_rank, name, highest_ranking, hwr_date, country, category) %>%
                        as_tibble()


  ## Women's rankings

    ## Ranking url
    rankings_url <- "http://www.squashinfo.com/rankings/women"

    ## Check URL for Robots.txt
    suppressMessages(session <- bow(rankings_url))

    ## Create womens_rankings
    womens_rankings <- c()

    for (i in 1:(round_any(top, 50, ceiling)/50)) {

      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)

      ## Scrape rankings table
      current_page <- suppressMessages(bow(rankings_url)) %>%
                                          scrape()

      results <- current_page %>%
                    html_nodes("table") %>%
                    html_table()

      ## Store results in mens_rankings
      womens_rankings <- bind_rows(womens_rankings, results)

    }

    ## Clean mens_rankings
    womens_rankings <- womens_rankings %>%
                          clean_names() %>%
                          filter(rank <= top) %>%
                          mutate(category = "Women's",
                                 date = ymd(parse_date_time(date, orders = "bY"))) %>%
                          rename(previous_rank = prev,
                                 highest_ranking = hwr,
                                 hwr_date = date) %>%
                          select(rank, previous_rank, name, highest_ranking, hwr_date, country, category) %>%
                          as_tibble()


    ## All rankings
    all_rankings <- bind_rows(mens_rankings, womens_rankings)

    return(all_rankings)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


}
