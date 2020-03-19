#' Get PSA Squash Player Data from SquashInfo
#'
#' Returns scraped profile data men and/or women players in PSA World Tour competitions
#'
#'
#' @param top integer indicating the number of top PSA players to return
#' 
#' @param category character string indicating the competition category
#'
#'
#' @return Tibble containing first name, last name, age, gender, birthplace, nationality, residence, height in cm, weight in kg, plays (handedness), racket brand, year of joining PSA, university, and club.
#'
#' @examples 
#' 
#' get_players(top = 25, category = "women")
#' 
#' both <- get_players(5, "both")
#'
#' @note This function only returns players ranked in the most recent PSA rankings table for Men's and Women's singles competitions.
#' 
#' @references 
#' 
#'     \url{http://www.squashinfo.com/rankings/men}
#'     \url{http://www.squashinfo.com/rankings/women}
#' 
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

get_players <- function(top = 25, category = c("both", "men", "women")) {
  
  stopifnot(is.numeric(top), is.character(category))
  
  
# Men's

  if (category == "men") {
    
      ## Get profile URLs for top n men
      rankings_url <- "http://www.squashinfo.com/rankings/men"
      
      ## Check URL for Robots.txt
      session <- bow(rankings_url)
      
      ## Create mens_profile_urls
      mens_profile_urls <- c()
      
      for (i in 1:(round_any(top, 50, ceiling)/50)) {
        
        ## Next tab in rankings table
        rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)
        
        ## Scrape table for player profile hrefs
        results <- read_html(rankings_url, encoding = "UTF-8") %>% 
                      html_nodes(xpath = "//td/a") %>% 
                      html_attr("href")
        
        ## Store hrefs in 
        mens_profile_urls <- c(mens_profile_urls, results)
        
      }
      
      ## Scrape profile info for top n mens players
      
      ## Create mens_profiles
      mens_profiles <- c()
      
      for (i in 1:top) {
        
        profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls[i])
        
        ## Verbose
        message("Scraping ", profile_url)
        
        ## Extract player name from profile header
        player_name <- read_html(profile_url, encoding = "UTF-8") %>%
                          html_nodes("h1")
        
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
        first <- gsub(" .*","\\1", player_name)
        last <- gsub(".* ","\\2", player_name)
        
        ## Extract player profile info
        result <- read_html(profile_url, encoding = "UTF-8") %>%
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
                      spread(key = rowid, value = value)
        
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
                      mutate(first = first, last = last, nationality = nationality) %>%
                      select(first, last, age, gender, birthplace, nationality, residence, height, weight, plays, racket, joined_psa, coach, university, club)
        
        ## Bind player profile to mens_profiles
        mens_profiles <- bind_rows(mens_profiles, metrics)
        
      }
      
      mens_profiles <- mens_profiles %>%
                          select(-coach) %>%
                          mutate(plays = if_else(str_detect(plays, regex("right", ignore_case = TRUE)), "R", if_else(str_detect(plays, regex("left", ignore_case = TRUE)), "L", NA_character_)),
                                 height = as.numeric(gsub("cm.*","\\1", height)),
                                 weight = as.numeric(gsub("kg.*","\\1", weight)),
                                 age = as.numeric(age),
                                 joined_psa = as.numeric(joined_psa))
      
      return(mens_profiles)
      
      
# Womens
      
      
  } else if (category == "women") {
    
    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"
    
    ## Check URL for Robots.txt
    session <- bow(rankings_url)
    
    ## Create womens_profile_urls
    womens_profile_urls <- c()
    
    for (i in 1:(round_any(top, 50, ceiling)/50)) {
      
      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)
      
      
      ## Scrape table for player profile hrefs
      results <- read_html(rankings_url, encoding = "UTF-8") %>% 
                    html_nodes(xpath = "//td/a") %>% 
                    html_attr("href")
      
      womens_profile_urls <- c(womens_profile_urls, results)
      
    }
    
    ## Scrape profile info for top n womens players
    
    ## Create mens_profiles
    womens_profiles <- c()
    
    for (i in 1:top) {
      
      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls[i])
      
      ## Verbose
      message("Scraping ", profile_url)
      
      ## Extract player name from profile header
      player_name <- read_html(profile_url, encoding = "UTF-8") %>%
                        html_nodes("h1")
      
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
      first <- gsub(" .*","\\1", player_name)
      last <- gsub(".* ","\\2", player_name)
      
      ## Extract player profile info
      result <- read_html(profile_url, encoding = "UTF-8") %>%
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
                    spread(key = rowid, value = value)
      
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
                    mutate(first = first, last = last, nationality = nationality) %>%
                    select(first, last, age, gender, birthplace, nationality, residence, height, weight, plays, racket, joined_psa, coach, university, club)
      
      ## Bind player profile to womens_profiles
      womens_profiles <- bind_rows(womens_profiles, metrics)
      
    }
    
    womens_profiles <- womens_profiles %>%
                          select(-coach) %>%
                          mutate(plays = if_else(str_detect(plays, regex("right", ignore_case = TRUE)), "R", if_else(str_detect(plays, regex("left", ignore_case = TRUE)), "L", NA_character_)),
                                 height = as.numeric(gsub("cm.*","\\1", height)),
                                 weight = as.numeric(gsub("kg.*","\\1", weight)),
                                 age = as.numeric(age),
                                 joined_psa = as.numeric(joined_psa)) 
    
    return(womens_profiles)
  
    
## Both
    
  
  } else if (category == "both") {
    
    
  ## Men
    
    ## Get profile URLs for top n men
    rankings_url <- "http://www.squashinfo.com/rankings/men"
    
    ## Check URL for Robots.txt
    session <- bow(rankings_url)
    
    ## Create mens_profile_urls
    mens_profile_urls <- c()
    
    for (i in 1:(round_any(top, 50, ceiling)/50)) {
      
      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/men/%s", i)
      
      
      ## Scrape table for player profile hrefs
      results <- read_html(rankings_url, encoding = "UTF-8") %>% 
                    html_nodes(xpath = "//td/a") %>% 
                    html_attr("href")
      
      ## Store hrefs in 
      mens_profile_urls <- c(mens_profile_urls, results)
      
    }
    
    ## Scrape profile info for top n mens players
    
    ## Create mens_profiles
    mens_profiles <- c()
    
    for (i in 1:top) {
      
      profile_url <- sprintf("http://www.squashinfo.com%s", mens_profile_urls[i])
      
      ## Verbose
      message("Scraping ", profile_url)
      
      ## Extract player name from profile header
      player_name <- read_html(profile_url, encoding = "UTF-8") %>%
                        html_nodes("h1")
      
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
      first <- gsub(" .*","\\1", player_name)
      last <- gsub(".* ","\\2", player_name)
      
      ## Extract player profile info
      result <- read_html(profile_url, encoding = "UTF-8") %>%
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
                      spread(key = rowid, value = value)
      
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
                    mutate(first = first, last = last, nationality = nationality) %>%
                    select(first, last, age, gender, birthplace, nationality, residence, height, weight, plays, racket, joined_psa, coach, university, club)
      
      ## Bind player profile to mens_profiles
      mens_profiles <- bind_rows(mens_profiles, metrics)
    
    }
    
    mens_profiles <- mens_profiles %>%
                          select(-coach) %>%
                          mutate(plays = if_else(str_detect(plays, regex("right", ignore_case = TRUE)), "R", if_else(str_detect(plays, regex("left", ignore_case = TRUE)), "L", NA_character_)),
                                 height = as.numeric(gsub("cm.*","\\1", height)),
                                 weight = as.numeric(gsub("kg.*","\\1", weight)),
                                 age = as.numeric(age),
                                 joined_psa = as.numeric(joined_psa))
    
    
  ## Women
    
    ## Get profile URLs for top n women
    rankings_url <- "http://www.squashinfo.com/rankings/women"
    
    ## Check URL for Robots.txt
    session <- bow(rankings_url)
    
    ## Create womens_profile_urls
    womens_profile_urls <- c()
    
    for (i in 1:(round_any(top, 50, ceiling)/50)) {
      
      ## Next tab in rankings table
      rankings_url <- sprintf("http://www.squashinfo.com/rankings/women/%s", i)
      
      
      ## Scrape table for player profile hrefs
      results <- read_html(rankings_url, encoding = "UTF-8") %>% 
                    html_nodes(xpath = "//td/a") %>% 
                    html_attr("href")
      
      womens_profile_urls <- c(womens_profile_urls, results)
      
    }
    
    ## Scrape profile info for top n womens players
    
    ## Create mens_profiles
    womens_profiles <- c()
    
    for (i in 1:top) {
      
      profile_url <- sprintf("http://www.squashinfo.com%s", womens_profile_urls[i])
      
      ## Verbose
      message("Scraping ", profile_url)
      
      ## Extract player name from profile header
      player_name <- read_html(profile_url, encoding = "UTF-8") %>%
                          html_nodes("h1")
      
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
      first <- gsub(" .*","\\1", player_name)
      last <- gsub(".* ","\\2", player_name)
      
      ## Extract player profile info
      result <- read_html(profile_url, encoding = "UTF-8") %>%
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
                    spread(key = rowid, value = value)
      
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
                    mutate(first = first, last = last, nationality = nationality) %>%
                    select(first, last, age, gender, birthplace, nationality, residence, height, weight, plays, racket, joined_psa, coach, university, club)
      
      ## Bind player profile to womens_profiles
      womens_profiles <- bind_rows(womens_profiles, metrics)
      
    }
    
    womens_profiles <- womens_profiles %>%
                            select(-coach) %>%
                            mutate(plays = if_else(str_detect(plays, regex("right", ignore_case = TRUE)), "R", if_else(str_detect(plays, regex("left", ignore_case = TRUE)), "L", NA_character_)),
                                   height = as.numeric(gsub("cm.*","\\1", height)),
                                   weight = as.numeric(gsub("kg.*","\\1", weight)),
                                   age = as.numeric(age),
                                   joined_psa = as.numeric(joined_psa)) 
    
  ## Combine women and men profiles 
    
    both_profiles <- bind_rows(mens_profiles, womens_profiles)
    
    return(both_profiles)
    
    
  } else {
    
    stop("category not one of 'both', 'men', or 'women'")
    
  }
    
    
}
