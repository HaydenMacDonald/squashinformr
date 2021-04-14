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
#' @export

get_rankings <- function(top = NULL, category = NULL) {

  ## Stop if top is not numeric or NULL, or if category is not character
  stopifnot(is.numeric(top) | is.null(top), is.character(category))

  ## Make category input lowercase
  category <- tolower(category)

  # Men's
  if (category == "mens") {

    mens_rankings <- get_rankings_table(top = top, category = category)

  } else if (category == "womens") {

    womens_rankings <- get_rankings_table(top = top, category = category)

  } else if (category == "both") {

    mens_rankings <- get_rankings_table(top = top, category = "mens")

    womens_rankings <- get_rankings_table(top = top, category = "womens")

    ## Combine rankings tables
    all_rankings <- bind_rows(mens_rankings, womens_rankings)

    return(all_rankings)

  } else {

    stop("category must be one of 'both', 'mens', or 'womens'")

  }


}


#' Get the PSA rankings tables from SquashInfo
#'
#' Given a competition category, \code{get_rankings_table()} returns the most recent PSA rankings table.
#'
#' @param top integer indicating the number of top PSA players by rank to return.
#'
#' @param category character string indicating the competition category. Must be one of "mens", or "womens".
#'
#' @return Tibble containing the player rank, previous month's rank, name, highest ranking achieved, date of highest ranking, nationality, and competition category.
#'
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

get_rankings_table <- function(top = NULL, category = NULL) {

  ## Get profile URLs for top n players
  if (category == "mens") {
    rankings_url <- "http://www.squashinfo.com/rankings/men"
  } else if (category == "womens") {
    rankings_url <- "http://www.squashinfo.com/rankings/women"
  }

  ## Check URL for Robots.txt
  suppressMessages(session <- bow(rankings_url))

  ## Create rankings
  rankings <- NULL

  for (i in 1:(round_any(top, 50, ceiling)/50)) {

    ## Next tab in rankings table
    rankings_url <- sprintf(paste0(rankings_url, "/%s"), i)

    ## Scrape rankings page
    current_page <- suppressMessages(bow(rankings_url)) %>%
      scrape()

    ## Extract rankings table
    results <- current_page %>%
      html_nodes("table") %>%
      html_table()

    ## Store results in rankings
    rankings <- bind_rows(rankings, results)

  }

  ## Clean rankings
  rankings <- rankings %>%
    clean_names() %>%
    filter(rank <= top) %>% ## Filter out extraneous players
    mutate(category = category, ## Add category column
           date = ymd(parse_date_time(date, orders = "bY"))) %>% ## Clean date
    rename(previous_rank = prev,
           highest_world_ranking = hwr,
           hwr_date = date) %>%
    select(rank, previous_rank, name, highest_world_ranking, hwr_date, country, category) %>%
    as_tibble()

  return(rankings)

}
