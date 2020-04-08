#' Get a tournament's games from SquashInfo
#'
#' Given a tournament name or a year, \code{get_tournament_games()} returns match data for PSA World Tour tournaments and other events.
#'
#' @param tournament character string of name of the tournament. Partial tournament names are matched via regular expressions.
#'
#' @param year integer indicating year of competition. Must be 2019 or 2020.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the tournament name, competition category, tournament date, round, match number, game number, player 1, player 2, the game winner, player 1's score, player 2's score, player 1's seed, player 2's seed, player 1's nationality, player 2's nationality.
#'
#' @examples
#' ## Return game data for 2019's Tournament of Champions.
#' get_tournament_games("tournament of champions", year = 2019)
#'
#' ## Return game data for all PSA World Tour tournaments in 2019
#' get_tournament_games(year = 2019)
#'
#' ## Return game data for ALL tournaments in 2020
#' get_tournament_games(year = 2020, world_tour = FALSE)
#'
#'
#' @note This function only returns game data from tournaments in 2019 and 2020, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
#'
#' @references
#'
#'     \url{http://www.squashinfo.com/results}
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
#' @importFrom dplyr desc
#' @importFrom dplyr arrange
#' @importFrom tidyr unnest
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
#' @importFrom lubridate year
#' @importFrom lubridate dmy
#' @importFrom janitor clean_names
#' @importFrom naniar replace_with_na
#' @importFrom stats setNames
#'
#' @export

get_tournament_games <- function(tournament = NULL, year = 2020, world_tour = TRUE) {

  stopifnot(is.character(tournament) | is.null(tournament), is.numeric(year) | is.null(year), is.logical(world_tour))

  if (!is.null(tournament)) {

    if (str_detect(tournament, pattern = regex("Premier League", ignore_case = TRUE)) == TRUE) {

      stop("Premier League data are currently not available through squashinformr")

    }

  }

  if (is.null(year) == FALSE) {

    if(year != 2019 & year != 2020) {

      stop("Year must be either 2019 or 2020")

    }

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
                  .[[2]] %>%
                  html_table(header = TRUE) %>%
                  as.data.frame() %>%
                  clean_names() %>%
                  rename(league = x) %>%
                  mutate(date = dmy(date)) %>%
                  filter(!str_detect(name, pattern = "Premier League"))

    slug <- current_page %>%
              html_nodes(xpath = "//td/a") %>%
              html_attr("href") %>%
              .[str_detect(., "events")] %>%
              .[!str_detect(., "premier-league")]

    results <- cbind(results, slug)


    if (world_tour == TRUE) {

      results <- results %>%
                    filter(str_detect(league, pattern = "World"))

    }


    tournaments <- rbind(tournaments, results)


    if (is.null(tournament) == FALSE & is.null(year) == TRUE) {

      if (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE))) {

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

    } else if (is.null(tournament) == TRUE & is.null(year) == FALSE) {

      if ((year - 1) %in% year(tournaments$date)) {

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

    } else {


      if ((year %in% year(tournaments[str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)), ]$date)) & (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)))) {

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

  } # end while loop


  ## Filter tournaments according to tournament name and/or year

  if (is.null(tournament) == TRUE) {

    tournaments <- tournaments %>%
                      filter(year(date) >= year)

  } else if (is.null(year) == TRUE) {

    tournaments <- tournaments %>%
                      filter(str_detect(name, regex(tournament, ignore_case = TRUE)))

  } else if (is.null(tournament) == FALSE & is.null(year) == FALSE) {

    tournaments <- tournaments %>%
                      filter(year(date) == year, str_detect(name, regex(tournament, ignore_case = TRUE)))

  }


  games <- c()

  for (i in 1:length(tournaments$slug)) {

    t_url <- sprintf("http://www.squashinfo.com%s", tournaments$slug[i])

    ## Verbose
    message("Scraping ", t_url)

    t_name <- tournaments$name[i]

    t_category <- if_else(str_detect(t_name, pattern = regex(" \\(M\\)")), "Men's", if_else(str_detect(t_name, pattern = regex(" \\(W\\)")), "Women's", NA_character_))

    t_name <- str_replace_all(t_name, pattern = regex(" \\(M\\)"), "")
    t_name <- str_replace_all(t_name, pattern = regex(" \\(W\\)"), "")

    t_date <- tournaments$date[i]

    current_page <- suppressMessages(bow(t_url, verbose = FALSE) %>%
                                       scrape(verbose = FALSE))

    result <- current_page %>%
                  html_nodes("table") %>%
                  html_table() %>%
                  as.data.frame()

    result <- result %>%
                replace_with_na(replace = list(X1 = "", X2 = ""))

    result <- result[rowSums(is.na(result)) != ncol(result),]

    lags <- seq(1, dim(result)[1], 1)

    lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")

    lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

    lag_df <- result %>%
                mutate_at(vars(X1), suppressWarnings(funs_(lag_functions)))

    result$round <- NA

    for (i in 1:length(result$X1)) {

      if ("1st round:" %in% lag_df[i,]) {

        result$round[i] <- 1

      } else if ("2nd round:" %in% lag_df[i,]) {

        result$round[i] <- 2

      } else if ("3rd round:" %in% lag_df[i,]) {

        result$round[i] <- 3

      } else if ("Quarter-finals:" %in% lag_df[i,]) {

        result$round[i] <- 4

      } else if ("Semi-finals:" %in% lag_df[i,]) {

        result$round[i] <- 5

      } else if ("Third place play-off:" %in% lag_df[i,]) {

        result$round[i] <- 6

      } else if ("Final:" %in% lag_df[i,]) {

        result$round[i] <- 7

      }

    }

    result <- result %>%
                filter(X1 != "Final:", X1 != "Semi-finals:", X1 != "Quarter-finals:", X1 != "3rd round:", X1 != "2nd round:", X1 != "1st round:") %>%
                mutate(X1 = str_replace_all(X1, pattern = regex("[:punct:]"), replacement = ""),
                       player_2 = str_trim(if_else(X2 == "bye", "bye", if_else(str_detect(X1, " v "), str_extract(X1, pattern = "(?<= v).*"), str_extract(X1, pattern = "(?<= bt).*"))), side = "both"),
                       player_1 = str_trim(if_else(X2 == "bye", X1, if_else(str_detect(X1, " v "), str_extract(X1, pattern = ".*(?= v)"), str_extract(X1, pattern = ".*(?= bt)"))), side = "both"),
                       player_1_seed = as.numeric(str_extract(player_1, pattern = regex("^[:digit:]{1,}"))),
                       player_2_seed = as.numeric(str_extract(player_2, pattern = regex("^[:digit:]{1,}"))),
                       player_1 = str_remove(player_1, pattern = regex("^[:digit:]{1,}")),
                       player_2 = str_remove(player_2, pattern = regex("^[:digit:]{1,}")),
                       player_1_nationality = str_extract(player_1, pattern = regex("[A-Z]{2,}$")),
                       player_2_nationality = str_extract(player_2, pattern = regex("[A-Z]{2,}$")),
                       player_1 = str_remove(player_1, pattern = regex("^WC ")),
                       player_2 = str_remove(player_2, pattern = regex("^WC ")),
                       player_1 = str_trim(str_remove(player_1, pattern = regex("[A-Z]{2,}$")), side = "both"),
                       player_2 = str_trim(str_remove(player_2, pattern = regex("[A-Z]{2,}$")), side = "both"),
                       match_winner = if_else(X2 == "bye", NA_character_, if_else(str_detect(X1, " v "), "TBD", player_1)),
                       match_time = if_else(X2 == "bye", NA_character_, str_extract(X2, pattern = regex("[:digit:]{2,}m"))),
                       match_time = as.numeric(str_remove(match_time, pattern = regex("m", ignore_case = TRUE))),
                       X2 = if_else(str_detect(X2, " v "), NA_character_, str_replace_all(X2, pattern = regex(" \\([:digit:]{2,}m\\)$"), replacement = "")),
                       round = factor(round,
                                      labels = if (length(unique(result$round)) == 6) {c("1st", "2nd", "3rd", "QF", "SF", "F")}
                                      else if (6 %in% result$round & length(unique(result$round)) == 8) {c("1st", "2nd", "3rd", "4th", "QF", "SF", "3rd place match", "F")}
                                      else if (6 %in% result$round & length(unique(result$round)) == 7) {c("1st", "2nd", "3rd", "QF", "SF", "3rd place match", "F")}
                                      else if (6 %in% result$round & length(unique(result$round)) == 6) {c("1st", "2nd", "QF", "SF", "3rd place match", "F")}
                                      else if (6 %in% result$round & length(unique(result$round)) == 5) {c("2nd", "QF", "SF", "3rd place match", "F")}
                                      else if (length(unique(result$round)) == 5) {c("1st", "2nd", "QF", "SF", "F")}
                                      else if (6 %in% result$round & length(unique(result$round)) == 4) {c("QF", "SF", "3rd place match", "F")}
                                      else if (length(unique(result$round)) == 4) {c("1st", "QF", "SF", "F")}
                                      else if (length(unique(result$round)) == 3) {c("QF", "SF", "F")}
                                      else if (length(unique(result$round)) == 2) {c("SF", "F")}
                                      else if (length(unique(result$round)) == 1) {c("F")},
                                      ordered = TRUE),
                       tournament_name = t_name,
                       category = t_category,
                       tournament_date = t_date) %>%
                select(tournament_name, category, tournament_date, player_1, player_2, match_winner, match_time, games = X2, player_1_seed, player_2_seed, player_1_nationality, player_2_nationality, round) %>%
                mutate(match = nrow(.) - row_number())

    result <- result %>%
                mutate(games = strsplit(as.character(games), ", ")) %>%
                unnest(games) %>%
                group_by(tournament_name, round, player_1, player_2, match_time) %>%
                mutate(game = row_number()) %>%
                ungroup() %>%
                mutate(player_1_score = as.numeric(str_extract(games, pattern = "^[:digit:]+(?=\\-)")),
                       player_2_score = as.numeric(str_extract(games, pattern = "(?<=\\-)[:digit:]+")),
                       game_winner = if_else(player_1_score > player_2_score, player_1, player_2)) %>%
                select(-match_winner, -match_time, -games)

    games <- rbind(games, result)

  }


  games <- games %>%
              select(tournament_name, category, tournament_date, round, match, game, player_1, player_2, game_winner, player_1_score, player_2_score, player_1_seed, player_2_seed, player_1_nationality, player_2_nationality) %>%
              arrange(tournament_date, desc(round), desc(match), desc(game))


  return(games)


}
