#' Get PSA Squash Player Data from SquashInfo
#'
#' Given the rank(s) and competition category, \code{get_players()} returns profile data of ranked players in PSA World Tour competitions.
#'
#' @param top integer indicating the number of top PSA players by rank to return.
#' @param rank integer indicating the rank of the PSA player(s) to return.
#' @param category character string indicating the competition category. Must be one of "mens", "womens", or "both".
#'
#' @return Tibble containing first name, last name, age, gender, birthplace, nationality, residence, height in cm, weight in kg, plays (handedness), racket brand, year of joining PSA, university, and club.
#'
#' @examples
#' ## Return the top 5 ranked players from the Women's PSA rankings
#' \donttest{get_players(top = 5, category = "womens")}
#'
#' ## Return the 5th ranked player from both Men's and Women's PSA rankings
#' \donttest{get_players(rank = 5, category = "both")}
#'
#' @note This function only returns players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @importFrom dplyr bind_rows
#'
#' @export

get_players <- function(top = NULL, rank = NULL, category = NULL) {

  ## Return error if top is not numeric or NULL, if category is not character or NULL, if rank is not numeric or NULL, or if rank's length is not not 1 or NULL
  stopifnot(is.numeric(top) | is.null(top), is.character(category) | is.null(category), is.numeric(rank) | is.null(rank), length(rank) == 1 | is.null(rank))

  ## Return error if both top and rank are provided or NULL
  if (sum(is.null(top), is.null(rank)) != 1) {
    stop("Please use one of rank or top arguments")
  }

  ## Convert category argument to lower case
  category <- tolower(category)

  # Men's
  if (category == "mens") {

    mens_profiles <- get_player_profiles(top = top, rank = rank, category = "mens")

    return(mens_profiles)

  # Women's
  } else if (category == "womens") {

    womens_profiles <- get_player_profiles(top = top, rank = rank, category = "womens")

    return(womens_profiles)

  # Both categories
  } else if (category == "both") {

    ## Men's and women's profiles
    mens_profiles <- get_player_profiles(top = top, rank = rank, category = "mens")
    womens_profiles <- get_player_profiles(top = top, rank = rank, category = "womens")

    ## Combine women and men profiles
    both_profiles <- bind_rows(mens_profiles, womens_profiles)

    return(both_profiles)


  } else {

    ## Return error when category is not one of "mens", "womens", or "both"
    stop("category must be one of 'both', 'mens', or 'womens'")

  }

}





#' Get Player Profiles from SquashInfo
#'
#' Given the rank(s) and competition category, \code{get_player_profiles()} returns profile data of ranked players in PSA World Tour competitions.
#'
#' @param top integer indicating the number of top PSA players by rank to return.
#' @param rank integer indicating the rank of the PSA player(s) to return.
#' @param category character string indicating the competition category. Must be one of "mens" or "womens".
#'
#' @return Tibble containing first name, last name, age, gender, birthplace, nationality, residence, height in cm, weight in kg, plays (handedness), racket brand, year of joining PSA, university, and club.
#'
#' @examples
#' ## Return the top 5 ranked players from the Women's PSA rankings
#' \donttest{get_player_profiles(top = 5, category = "womens")}
#'
#' ## Return the 5th ranked player from the Men's PSA rankings
#' \donttest{get_player_profiles(rank = 5, category = "mens")}
#'
#' @noRd
#'
#' @note This function only returns players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/rankings/men} \cr
#'     \url{http://www.squashinfo.com/rankings/women}
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom plyr round_any
#' @importFrom tidyr pivot_wider
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr regex
#' @importFrom tibble rowid_to_column
#' @importFrom tibble enframe

get_player_profiles <- function(category = c("mens", "womens"), top = NULL, rank = NULL) {

  ## Get profile URLs for top n players
  if (category == "mens") {
    rankings_url <- "http://www.squashinfo.com/rankings/men"
  } else if (category == "womens") {
    rankings_url <- "http://www.squashinfo.com/rankings/women"
  }

  ## Check URL for Robots.txt
  suppressMessages(session <- bow(rankings_url))

  ## Create profile_urls
  profile_urls <- NULL

  for (i in if (is.null(top) == TRUE) {1:(round_any(rank, 50, ceiling)/50)} else {1:(round_any(top, 50, ceiling)/50)}) {

    ## Next tab in rankings table
    rankings_url <- sprintf(paste0(rankings_url, "/%s"), i)

    ## Scrape table for player profile hrefs
    current_page <- suppressMessages(bow(rankings_url)) %>%
      scrape()

    results <- current_page %>%
      html_nodes(xpath = "//td/a") %>%
      html_attr("href")

    ## Store hrefs in
    profile_urls <- c(profile_urls, results)

  }

  ## Scrape profile info for top n players

  ## Create profiles
  profiles <- NULL

  for (i in if (is.null(top) == TRUE) {rank:rank} else {1:top}) {

    profile_url <- sprintf("http://www.squashinfo.com%s", profile_urls[i])

    ## Verbose
    message("Scraping ", profile_url)

    ## Read profile
    profile <- suppressMessages(bow(profile_url)) %>%
      scrape()

    ## Extract player name from profile header
    player_name <- profile %>%
      html_nodes("h1")

    ## Extract rank
    rank <- profile %>%
      html_nodes("div.content_column") %>%
      html_nodes(xpath = '//*[@id="world_ranking"]') %>%
      html_text() %>%
      as.numeric()

    ## Extract player nationality from profile header
    nationality <- player_name %>%
      html_nodes("span") %>%
      html_text(trim = TRUE) %>%
      str_replace_all("\\(", "") %>%
      str_replace_all("\\)", "")

    ## Clean player name
    player_name <- player_name %>%
      html_text(trim = TRUE) %>%
      str_replace_all(nationality, "") %>%
      str_replace_all("\\(", "") %>%
      str_replace_all("\\)", "") %>%
      str_replace_all(" $", "")

    ## Parse player name into first and last names
    first <- str_extract(player_name, "([^ ]+)")
    last <- str_trim(str_extract(player_name, " (.*)"), side = "left")

    ## Extract player profile info
    result <- profile %>%
      html_nodes("div.content_column") %>%
      html_nodes(xpath = '//*[@class="row"]')

    ## Extract player profile info
    result <- result %>%
      html_nodes("span") %>%
      html_text()

    ## Extract and clean variable names from profile info into vector
    vars <- result %>%
      .[c(TRUE, FALSE)] %>%
      str_replace_all(pattern = ":", replacement = "") %>%
      str_replace_all(pattern = " ", replacement = "_") %>%
      tolower()

    ## Extract and clean data from profile info into vector
    metrics <- result %>%
      .[c(FALSE, TRUE)] %>%
      str_replace_all(pattern = ":", replacement = "")

    ## Create data frame from profile data
    metrics <- tibble::enframe(metrics, name = NULL) %>%
      rowid_to_column() %>%
      pivot_wider(names_from = rowid, values_from = value)

    ## Assign variable names to profile data frame
    names(metrics) <- vars

    ## Normalize profile so that it contains NAs when profile does not contain particular variables
    metrics <- metrics %>%
      mutate(age = if ("age" %in% names(.)) {.$age} else {NA_real_},
             gender = if ("gender" %in% names(.)) {.$gender} else {NA_character_},
             birthplace = if ("birthplace" %in% names(.)) {.$birthplace} else {NA_character_},
             residence = if ("residence" %in% names(.)) {.$residence} else {NA_character_},
             height = if ("height" %in% names(.)) {.$height} else {NA_character_},
             weight = if ("weight" %in% names(.)) {.$weight} else {NA_character_},
             plays = if ("plays" %in% names(.)) {.$plays} else {NA_character_},
             joined_psa = if ("joined_psa" %in% names(.)) {.$joined_psa} else {NA_real_},
             coach = if ("coach" %in% names(.)) {.$coach} else {NA_character_},
             university = if ("university" %in% names(.)) {.$university} else {NA_character_},
             racket = if ("racket" %in% names(.)) {.$racket} else {NA_character_},
             club = if ("club" %in% names(.)) {.$club} else {NA_character_})

    ## Add first name, last name, nationality and arrange variables
    metrics <- metrics %>%
      mutate(rank = rank, first = first, last = last, nationality = nationality) %>%
      select(rank, first, last, age, gender, birthplace, nationality, residence, height, weight, plays, racket, joined_psa, coach, university, club)

    ## Bind player profile to profiles
    profiles <- bind_rows(profiles, metrics)

  }

  ## Clean profiles data frame
  profiles <- profiles %>%
    select(-coach) %>%
    mutate(plays = if_else(str_detect(plays, regex("right", ignore_case = TRUE)), "R", if_else(str_detect(plays, regex("left", ignore_case = TRUE)), "L", NA_character_)),
           height = as.numeric(gsub("cm.*","\\1", height)),
           weight = as.numeric(gsub("kg.*","\\1", weight)),
           age = as.numeric(age),
           joined_psa = as.numeric(joined_psa))

  return(profiles)

}


