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
      .[[1]] %>%
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




#' Get a tournament's players from SquashInfo
#'
#' Given a tournament name or a year, \code{get_tournament_players()} returns player registrants for PSA World Tour tournaments and other events.
#'
#' @param tournament character string of name of the tournament. Partial tournament names are matched via regular expressions.
#' @param year integer indicating year of competition. Must be 2020, 2021, or NULL if querying results for both years.
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the tournament name, competition category, tournament date, player, seed, nationality, round_reached.
#'
#' @examples
#' ## Who played in the Tournament of Champions in 2020?
#' \donttest{get_tournament_players(tournament = "tournament of champions", year = 2020)}
#'
#' ## Return player registrant data for all PSA World Tour tournaments in 2020
#' \donttest{get_tournament_players(year = 2020, world_tour = TRUE)}
#'
#' @note This function only returns player registrant data from tournaments in 2020 and 2021, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/results}
#'
#' @export

get_tournament_players <- function(tournament = NULL, year = 2021, world_tour = TRUE) {

  ## Get tournament
  tournaments <- get_tournament_(tournament = tournament, year = year, world_tour = world_tour)

  ## Scrape and clean tournament player data
  players <- get_tournament_objects(tournaments, level = "players")

  ## Return tibble of players
  return(players)

}




#' Get a tournament's matches from SquashInfo
#'
#' Given a tournament name or a year, \code{get_tournament_matches()} returns match data for PSA World Tour tournaments and other events.
#'
#' @param tournament character string of name of the tournament. Partial tournament names are matched via regular expressions.
#'
#' @param year integer indicating year of competition. Must be 2020, 2021, or NULL if querying results for both years.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the tournament name, competition category, tournament date, round, player 1, player 2, the match winner, games won (by player 1), games lost (by player 1), the match time, player 1's seed, player 2's seed, player 1's nationality, player 2's nationality.
#'
#' @examples
#' ## Return match data for 2020's Tournament of Champions.
#' \donttest{get_tournament_matches("tournament of champions", year = 2020, world_tour = TRUE)}
#'
#' ## Return match data for all PSA World Tour tournaments in 2020
#' \donttest{get_tournament_matches(year = 2020, world_tour = TRUE)}
#'
#'
#'
#' @note This function only returns match data from tournaments in 2020 and 2021, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
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
#' @importFrom dplyr arrange
#' @importFrom stringr str_replace_all
#' @importFrom stringr regex
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#'
#' @export

get_tournament_matches <- function(tournament = NULL, year = NULL, world_tour = NULL) {

  ## Get tournament
  tournaments <- get_tournament_(tournament = tournament, year = year, world_tour = world_tour)

  ## Scrape and clean tournament match data
  matches <- get_tournament_objects(tournaments, level = "matches")

  ## Clean match data
  matches <- matches %>%
    arrange(tournament_date, desc(round)) %>%  ## order rows by date and round
    mutate(
      games = str_replace_all(
        games,
        pattern = regex(" \\([:digit:]{2,}m\\)"),
        replacement = ""
      ),
      ## remove match time from games column
      games = str_extract_all(games, pattern = "[0-9]+\\-[0-9]+"),
      ## Extract scores from games column
      ## create games_won and games_lost columns
      games_won = NA_real_,
      games_lost = NA_real_
    )

  ## Filter out extraneous rows with NA in player_1 and player_2
  matches <- matches %>%
    filter(is.na(player_1) == FALSE &
             is.na(player_2) == FALSE)

  ## For each row in matches
  for (j in seq_along(matches$games)) {
    ## if there are no games, go to next row, else extract match
    if (length(matches$games[[j]]) == 0 |
        matches$match_winner[[j]] == "TBD") {
      next
    } else {
      match <- matches$games[[j]]
    }

    ## start with 0 game wins and game losses
    wins <- 0

    losses <- 0

    ## For each game in a match
    for (i in seq_along(match)) {
      ## If the score of player 1 is greater than player 2
      if (as.numeric(str_extract(match[i], pattern = "^[0-9]+")) > as.numeric(str_extract(match[i], pattern = "[0-9]+$"))) {
        ## Add one to player 1's game wins
        wins <- wins + 1

      } else {
        ## If the score of player 1 is less than player 2

        ## Add one to player 1's game losses
        losses <- losses + 1

      }


    }

    ## Assign total game wins to games_won variable in matches
    matches$games_won[j] <- wins

    ## Assign total game losses to games_lost variable in matches
    matches$games_lost[j] <- losses

  }

  ## Reorder final results
  matches <- matches %>%
    select(
      tournament_name,
      category,
      tournament_date,
      round,
      player_1,
      player_2,
      match_winner,
      games_won,
      games_lost,
      match_time,
      player_1_seed,
      player_2_seed,
      player_1_nationality,
      player_2_nationality
    )


  return(matches)


}




#' Get a tournament's games from SquashInfo
#'
#' Given a tournament name or a year, \code{get_tournament_games()} returns match data for PSA World Tour tournaments and other events.
#'
#' @param tournament character string of name of the tournament. Partial tournament names are matched via regular expressions.
#'
#' @param year integer indicating year of competition. Must be 2020, 2021, or NULL if querying results for both years.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the tournament name, competition category, tournament date, round, match number, game number, player 1, player 2, the game winner, player 1's score, player 2's score, player 1's seed, player 2's seed, player 1's nationality, player 2's nationality.
#'
#' @examples
#' ## Return game data for 2020's Tournament of Champions.
#' \donttest{get_tournament_games("tournament of champions", year = 2020, world_tour = TRUE)}
#'
#' ## Return game data for all PSA World Tour tournaments in 2020
#' \donttest{get_tournament_games(year = 2020, world_tour = TRUE)}
#'
#'
#' @note This function only returns game data from tournaments in 2020 and 2021, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/results}
#'
#' @export

get_tournament_games <- function(tournament = NULL, year = NULL, world_tour = NULL) {

  ## Get tournament
  tournaments <- get_tournament_(tournament = tournament, year = year, world_tour = world_tour)

  ## Scrape and clean tournament game data
  games <- get_tournament_objects(tournaments, level = "games")

  ## Return tibble of games
  return(games)

}


#' Scrape tournament metadata
#'
#'
#' @param tournament the name of the tournament.
#' @param year the year the tournament was held.
#' @param world_tour logical indicating whether the tournament was a part of the PSA World Tour.
#'
#' @return a data frame containing a specified tournament and its slug.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom polite bow
#' @importFrom polite nod
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @importFrom lubridate year
#' @importFrom lubridate dmy
#' @importFrom lubridate today
#' @importFrom lubridate %m-%
#' @importFrom janitor clean_names

get_tournament_ <- function(tournament = NULL, year = NULL, world_tour = NULL) {

  ## Input errors
  stopifnot(
    is.character(tournament) | is.null(tournament),
    is.numeric(year) | is.null(year),
    is.logical(world_tour) & (is.null(world_tour) == FALSE)
  )

  ## Stop if querying "Premier League"
  if (!is.null(tournament)) {
    if (str_detect(tournament, pattern = regex("Premier League", ignore_case = TRUE)) == TRUE) {
      stop("Premier League data are currently not available through squashinformr")
    }
  }

  ## Stop if year does not meet following requirements
  if (is.null(year) == FALSE) {
    ## Stop if year is not 2020 or 2021
    if (year != 2020 & year != 2021) {
      stop("Year must be either 2020 or 2021")
    }

    ## Test year for length of four digits and non-negativity
    stopifnot((nchar(trunc(abs(
      year
    ))) == 4 & year > 0))
  }


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
      .[[1]] %>%
      html_table(header = TRUE) %>%
      as.data.frame() %>%
      clean_names() %>%
      rename(league = x) %>%
      mutate(date = dmy(date)) %>%
      filter(!str_detect(name, pattern = "Premier League"))

    ## Extract tournament slugs
    slug <- current_page %>%
      html_nodes(xpath = "//td/a") %>%
      html_attr("href") %>%
      .[str_detect(., "events")] %>%
      .[!str_detect(., "premier-league")]

    ## Add slugs to results
    results <- cbind(results, slug)

    ## Filter out non-world tour tournaments
    if (world_tour == TRUE) {
      results <- results %>%
        filter(str_detect(league, pattern = "World"))

    }

    ## Bind results row-wise
    tournaments <- rbind(tournaments, results)

    ## If tournament is given and year is NOT given
    if (is.null(tournament) == FALSE & is.null(year) == TRUE) {
      ## and tournament name is present in current results
      if (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE))) {
        results_limit <- NA_character_ ## end while loop

      } else {
        ## otherwise continue to next page

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

      ## If tournament name is not given and year is given
    } else if (is.null(tournament) == TRUE &
               is.null(year) == FALSE) {
      ## and the previous year is present within current results
      if ((year - 1) %in% year(tournaments$date)) {
        results_limit <- NA_character_ ## end while loop

      } else {
        ## otherwise continue to next page

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

      ## If tournament AND year are given
    } else {
      ## and year AND tournament name is present in current results
      if ((year %in% year(tournaments[str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)),]$date)) &
          (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)))) {
        results_limit <- NA_character_  ## end while loop

      } else {
        ## otherwise continue to next page

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

  } # END OF WHILE LOOP


  ## Filter tournaments according to tournament name and/or year
  if (is.null(tournament) == TRUE) {

    tournaments <- tournaments %>%
      filter(year(date) == year,
             date >= lubridate::today() %m-% months(6))

  } else if (is.null(year) == TRUE) {

    tournaments <- tournaments %>%
      filter(str_detect(name, regex(tournament, ignore_case = TRUE)))

  } else if (is.null(tournament) == FALSE & is.null(year) == FALSE) {

    tournaments <- tournaments %>%
      filter(year(date) == year, str_detect(name, regex(tournament, ignore_case = TRUE)))

  }

  return(tournaments)

}



#' Get players, matches, or games from a given tournament
#'
#'
#' @param tournaments a data frame containing a set of tournaments
#' @param level the level of object to return. Must be one of "players", "matches", or "games".
#'
#' @return a tibble containing the tournament objects.
#'
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr row_number
#' @importFrom dplyr funs_
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom tidyr unnest
#' @importFrom polite bow
#' @importFrom polite scrape
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr regex
#' @importFrom stringr str_trim
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom janitor clean_names
#' @importFrom naniar replace_with_na
#' @importFrom stats setNames

get_tournament_objects <- function(tournaments = NULL, level = NULL) {

  ## Input errors
  if(level != "players" & level != "matches" & level != "games") {
    stop("level must be one of 'players', 'matches', or 'games'")
  }

  ## Create empty data frame
  data <- c()

  for (i in seq_along(tournaments$slug)) {

    ## For each tournament slug
    ## Create tournament url from slug
    t_url <- sprintf("http://www.squashinfo.com%s", tournaments$slug[i])

    ## Verbose
    message("Scraping ", t_url)

    ## Extract tournament name
    t_name <- tournaments$name[i]

    ## Extract tournament category
    t_category <- if_else(str_detect(t_name, pattern = regex(" \\(M\\)")), "Men's",
                          if_else(str_detect(t_name, pattern = regex(" \\(W\\)")), "Women's", NA_character_))

    ## Clean tournament name
    t_name <- str_replace_all(t_name, pattern = regex(" \\(M\\)"), "")
    t_name <- str_replace_all(t_name, pattern = regex(" \\(W\\)"), "")

    ## Extract tournament date
    t_date <- tournaments$date[i]

    ## Bow and scrape page
    current_page <- suppressMessages(bow(t_url, verbose = FALSE) %>%
                                       scrape(verbose = FALSE))

    ## Find html tables
    result <- current_page %>%
      html_nodes("table") %>%
      html_table() %>%
      as.data.frame()

    ## Replace empty strings with NAs
    result <- result %>%
      replace_with_na(replace = list(X1 = "", X2 = ""))

    ## Remove extraneous rows
    result <- result[rowSums(is.na(result)) != ncol(result), ]

    ## Create dataframe of lags for each match in order to determine which tournament round the match occurred in
    lags <- seq(1, dim(result)[1], 1)

    lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")

    lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

    lag_df <- result %>%
      mutate_at(vars(X1), suppressWarnings(funs_(lag_functions)))

    ## Create round variable
    result$round <- NA

    ## For each match in result, find the corresponding lagged 'round' row
    for (i in seq_along(result$X1)) {
      if ("1st round:" %in% lag_df[i, ]) {

        result$round[i] <- 1

      } else if ("2nd round:" %in% lag_df[i, ]) {

        result$round[i] <- 2

      } else if ("3rd round:" %in% lag_df[i, ]) {

        result$round[i] <- 3

      } else if ("Quarter-finals:" %in% lag_df[i, ]) {

        result$round[i] <- 4

      } else if ("Semi-finals:" %in% lag_df[i, ]) {

        result$round[i] <- 5

      } else if ("Third place play-off:" %in% lag_df[i, ]) {

        result$round[i] <- 6

      } else if ("Final:" %in% lag_df[i, ]) {

        result$round[i] <- 7

      }

    }

    ## Clean results
    result <- result %>%
      ## Remove 'round' rows
      filter(
        X1 != "Final:",
        X1 != "Third place play-off:",
        X1 != "Semi-finals:",
        X1 != "Quarter-finals:",
        X1 != "3rd round:",
        X1 != "2nd round:",
        X1 != "1st round:"
      ) %>%
      mutate(X1 = str_replace_all(X1, pattern = regex("[:punct:]"), replacement = ""),  ## Remove punctuation from match column

             ## Extract player names
             player_2 = str_trim(if_else(X2 == "bye", "bye", if_else(str_detect(X1, " v "), str_extract(X1, pattern = "(?<= v).*"), str_extract(X1, pattern = "(?<= bt).*"))), side = "both"),
             player_1 = str_trim(if_else(X2 == "bye", X1, if_else(str_detect(X1, " v "), str_extract(X1, pattern = ".*(?= v)"), str_extract(X1, pattern = ".*(?= bt)"))), side = "both"),

             ## Extract player seeds
             player_1_seed = as.numeric(str_extract(player_1, pattern = regex("^[:digit:]{1,}"))),
             player_2_seed = as.numeric(str_extract(player_2, pattern = regex("^[:digit:]{1,}"))),

             ## Remove player seed from player name
             player_1 = str_remove(player_1, pattern = regex("^[:digit:]{1,}")),
             player_2 = str_remove(player_2, pattern = regex("^[:digit:]{1,}")),

             ## Extract player nationalities
             player_1_nationality = str_extract(player_1, pattern = regex("[A-Z]{2,}$")),
             player_2_nationality = str_extract(player_2, pattern = regex("[A-Z]{2,}$")),

             ## Remove wild card seeding designation from player names
             player_1 = str_remove(player_1, pattern = regex("^WC ")),
             player_2 = str_remove(player_2, pattern = regex("^WC ")),

             ## Remove player nationalities from player names
             player_1 = str_trim(str_remove(player_1, pattern = regex("[A-Z]{2,}$")), side = "both"),
             player_2 = str_trim(str_remove(player_2, pattern = regex("[A-Z]{2,}$")), side = "both"),

             ## Make round an ordered factor based on the number of unique rounds and whether it includes a third place play-off round
             round = factor(round,
                            labels = if (length(unique(result$round)) == 6) {
                               c("1st", "2nd", "3rd", "QF", "SF", "F")
                              } else if (6 %in% result$round & length(unique(result$round)) == 8) {
                               c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "QF",
                                 "SF",
                                 "3rd place match",
                                 "F")
                              } else if (6 %in% result$round & length(unique(result$round)) == 7) {
                               c("1st", "2nd", "3rd", "QF", "SF", "3rd place match", "F")
                              } else if (6 %in% result$round & length(unique(result$round)) == 6) {
                               c("1st", "2nd", "QF", "SF", "3rd place match", "F")
                              } else if (6 %in% result$round & length(unique(result$round)) == 5) {
                               c("2nd", "QF", "SF", "3rd place match", "F")
                              } else if (length(unique(result$round)) == 5) {
                               c("1st", "2nd", "QF", "SF", "F")
                              } else if (6 %in% result$round & length(unique(result$round)) == 4) {
                               c("QF", "SF", "3rd place match", "F")
                              } else if (length(unique(result$round)) == 4) {
                               c("1st", "QF", "SF", "F")
                              } else if (length(unique(result$round)) == 3) {
                               c("QF", "SF", "F")
                              } else if (length(unique(result$round)) == 2) {
                               c("SF", "F")
                              } else if (length(unique(result$round)) == 1) {
                               c("F")
                              }, ordered = TRUE),
                            ## Add tournament name, category, date
                            tournament_name = t_name,
                            category = t_category,
                            tournament_date = t_date)

    if (level == "players") {

      result <- result %>%
        select(
          player_1,
          player_2,
          player_1_seed,
          player_2_seed,
          player_1_nationality,
          player_2_nationality,
          round
        )

      ## Extract player_1 data in each match
      player_1 <- result %>%
        select(
          player = player_1,
          player_seed = player_1_seed,
          player_nationality = player_1_nationality,
          round
        )

      ## Extract player_2 data in each match
      player_2 <- result %>%
        select(
          player = player_2,
          player_seed = player_2_seed,
          player_nationality = player_2_nationality,
          round
        )

      ## Bind results row-wise and find unique players in tournament
      result <- bind_rows(player_1, player_2) %>%
        group_by(player) %>%
        summarize(
          seed = suppressWarnings(max(player_seed, na.rm = TRUE)),
          nationality = suppressWarnings(max(player_nationality, na.rm = TRUE)),
          round_reached = suppressWarnings(max(round, na.rm = TRUE)) ## find highest round reached by player in that tournament
        ) %>%
        filter(player != "bye") %>%
        ungroup() %>%
        mutate(seed = if_else(is.infinite(seed), NA_real_, if_else(seed == 916, 16, seed))) %>% ## Give seed 16 to player's given seed '9-16'
        arrange(seed)

      ## Add tournament name, category, date
      result <- result %>%
        mutate(
          tournament = t_name,
          category = t_category,
          tournament_date = t_date
        ) %>%
        select(tournament,
               category,
               tournament_date,
               player,
               seed,
               nationality,
               round_reached)

    }
    else if (level == "matches" | level == "games") {

      ## Make results into a tibble
      result <- result %>%
        mutate(## Extract match winner
               match_winner = if_else(X2 == "bye", NA_character_, if_else(str_detect(X1, " v "), "TBD", player_1)),

               ## Extract match time
               match_time = if_else(X2 == "bye", NA_character_, str_extract(X2, pattern = regex("[:digit:]{2,}m"))),
               match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),

               ## Remove match time from match column
               X2 = if_else(str_detect(X2, " v "), NA_character_, str_replace_all(X2, pattern = regex(" \\([:digit:]{2,}m\\)$"), replacement = ""))) %>%
        as_tibble()

      result <- result %>%
        select(
          tournament_name,
          category,
          tournament_date,
          player_1,
          player_2,
          match_winner,
          match_time,
          games = X2,
          player_1_seed,
          player_2_seed,
          player_1_nationality,
          player_2_nationality,
          round
        )

    }


    if (level == "games") {

      ## Generate match number
      result <- result %>%
        mutate(match = nrow(.) - row_number())

      ## Clean results to get game level data
      result <- result %>%
        mutate(games = strsplit(as.character(games), ", ")) %>% ## separate games into list elements
        unnest(games) %>% ## unnest list elements into rows
        group_by(tournament_name, round, player_1, player_2, match_time) %>%
        mutate(game = row_number()) %>%  ## generate game number
        ungroup() %>%
        mutate(
          player_1_score = as.numeric(str_extract(games, pattern = "^[:digit:]+(?=\\-)")), ## Extract player 1's score
          player_2_score = as.numeric(str_extract(games, pattern = "(?<=\\-)[:digit:]+")), ## Extract player 2's score
          game_winner = if_else(player_1_score > player_2_score, player_1, player_2)) %>%  ## Extract game winner based on player scores
        select(-match_winner,-match_time,-games)

    }

    ## Bind results row-wise
    data <- rbind(data, result)

  }

  return(data)

}
