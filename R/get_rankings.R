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
#' @return Tibble containing the player rank, player name, player's seeding, round reached, event name, event date, event location, and event tour.
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

get_rankings <- function(top = NULL, category = NULL) {

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
      results <- read_html(rankings_url) %>%
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
      results <- read_html(rankings_url) %>%
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
      results <- read_html(rankings_url) %>%
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
      results <- read_html(rankings_url) %>%
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


    ## All rankings
    all_rankings <- bind_rows(mens_rankings, womens_rankings)

    return(all_rankings)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


}
