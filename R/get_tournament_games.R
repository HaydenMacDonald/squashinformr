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
#' \donttest{get_tournament_games("tournament of champions", year = 2020)}
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

get_tournament_games <-
  function(tournament = NULL,
           year = 2021,
           world_tour = TRUE) {
    ## Input errors
    stopifnot(
      is.character(tournament) |
        is.null(tournament),
      is.numeric(year) | is.null(year),
      is.logical(world_tour)
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
        .[[2]] %>%
        html_table(header = TRUE) %>%
        as.data.frame() %>%
        clean_names() %>%
        rename(league = x) %>%
        mutate(date = dmy(date)) %>%
        filter(!str_detect(name, pattern = "Premier League"))

      ## Extract tournament slug
      slug <- current_page %>%
        html_nodes(xpath = "//td/a") %>%
        html_attr("href") %>%
        .[str_detect(., "events")] %>%
        .[!str_detect(., "premier-league")]

      ## Add slug to results
      results <- cbind(results, slug)

      ## Filter out non-world tour tournaments
      if (world_tour == TRUE) {
        results <- results %>%
          filter(str_detect(league, pattern = "World"))

      }


      ## Bind results row-wise
      tournaments <- rbind(tournaments, results)


      ## If tournament name is given and year is NOT given
      if (is.null(tournament) == FALSE & is.null(year) == TRUE) {
        ## and if tournament name is present in current results
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

        ## If tournament name is NOT given and year is given
      } else if (is.null(tournament) == TRUE &
                 is.null(year) == FALSE) {
        ## and if the previous year is present in current results
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


        ## If tournament name AND year are given
      } else {
        ## and tournament name AND year are present in current results
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
        filter(year(date) >= year)

    } else if (is.null(year) == TRUE) {
      tournaments <- tournaments %>%
        filter(str_detect(name, regex(tournament, ignore_case = TRUE)))

    } else if (is.null(tournament) == FALSE &
               is.null(year) == FALSE) {
      tournaments <- tournaments %>%
        filter(year(date) == year, str_detect(name, regex(tournament, ignore_case = TRUE)))

    }

    ## Create empty games object
    games <- c()

    for (i in seq_along(tournaments$slug)) {
      ## For every tournament slug

      ## Create tournament slug
      t_url <-
        sprintf("http://www.squashinfo.com%s", tournaments$slug[i])

      ## Verbose
      message("Scraping ", t_url)

      ## Extract tournament name
      t_name <- tournaments$name[i]

      ## Extract tournament category
      t_category <-
        if_else(
          str_detect(t_name, pattern = regex(" \\(M\\)")),
          "Men's",
          if_else(
            str_detect(t_name, pattern = regex(" \\(W\\)")),
            "Women's",
            NA_character_
          )
        )

      ## Clean tournament name
      t_name <-
        str_replace_all(t_name, pattern = regex(" \\(M\\)"), "")
      t_name <-
        str_replace_all(t_name, pattern = regex(" \\(W\\)"), "")

      ## Extract tournament date
      t_date <- tournaments$date[i]

      ## Bow and scrape page
      current_page <- suppressMessages(bow(t_url, verbose = FALSE) %>%
                                         scrape(verbose = FALSE))

      ## Find html table
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

      lag_names <-
        paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")

      lag_functions <-
        setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

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


      ## Clean results data
      result <- result %>%
        filter(
          X1 != "Final:",
          X1 != "Semi-finals:",
          X1 != "Quarter-finals:",
          X1 != "3rd round:",
          X1 != "2nd round:",
          X1 != "1st round:"
        ) %>% ## Remove 'round' rows
        mutate(
          X1 = str_replace_all(
            X1,
            pattern = regex("[:punct:]"),
            replacement = ""
          ),
          ## Remove punctuation from match column

          ## Extract player names from match column
          player_2 = str_trim(if_else(
            X2 == "bye",
            "bye",
            if_else(
              str_detect(X1, " v "),
              str_extract(X1, pattern = "(?<= v).*"),
              str_extract(X1, pattern = "(?<= bt).*")
            )
          ), side = "both"),
          player_1 = str_trim(if_else(
            X2 == "bye",
            X1,
            if_else(
              str_detect(X1, " v "),
              str_extract(X1, pattern = ".*(?= v)"),
              str_extract(X1, pattern = ".*(?= bt)")
            )
          ), side = "both"),

          ## Extract player seeds
          player_1_seed = as.numeric(str_extract(
            player_1, pattern = regex("^[:digit:]{1,}")
          )),
          player_2_seed = as.numeric(str_extract(
            player_2, pattern = regex("^[:digit:]{1,}")
          )),

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

          ## Extract match winner
          match_winner = if_else(
            X2 == "bye",
            NA_character_,
            if_else(str_detect(X1, " v "), "TBD", player_1)
          ),

          ## Extract match time
          match_time = if_else(X2 == "bye", NA_character_, str_extract(X2, pattern = regex(
            "[:digit:]{2,}m"
          ))),
          match_time = as.numeric(str_remove(
            match_time, pattern = regex("m", ignore_case = TRUE)
          )),

          ## Remove match time from match column
          X2 = if_else(
            str_detect(X2, " v "),
            NA_character_,
            str_replace_all(
              X2,
              pattern = regex(" \\([:digit:]{2,}m\\)$"),
              replacement = ""
            )
          ),

          ## Make round an ordered factor based on the number of unique rounds and whether it includes a third place play-off round
          round = factor(
            round,
            labels = if (length(unique(result$round)) == 6) {
              c("1st", "2nd", "3rd", "QF", "SF", "F")
            }
            else if (6 %in% result$round &
                     length(unique(result$round)) == 8) {
              c("1st",
                "2nd",
                "3rd",
                "4th",
                "QF",
                "SF",
                "3rd place match",
                "F")
            }
            else if (6 %in% result$round &
                     length(unique(result$round)) == 7) {
              c("1st", "2nd", "3rd", "QF", "SF", "3rd place match", "F")
            }
            else if (6 %in% result$round &
                     length(unique(result$round)) == 6) {
              c("1st", "2nd", "QF", "SF", "3rd place match", "F")
            }
            else if (6 %in% result$round &
                     length(unique(result$round)) == 5) {
              c("2nd", "QF", "SF", "3rd place match", "F")
            }
            else if (length(unique(result$round)) == 5) {
              c("1st", "2nd", "QF", "SF", "F")
            }
            else if (6 %in% result$round &
                     length(unique(result$round)) == 4) {
              c("QF", "SF", "3rd place match", "F")
            }
            else if (length(unique(result$round)) == 4) {
              c("1st", "QF", "SF", "F")
            }
            else if (length(unique(result$round)) == 3) {
              c("QF", "SF", "F")
            }
            else if (length(unique(result$round)) == 2) {
              c("SF", "F")
            }
            else if (length(unique(result$round)) == 1) {
              c("F")
            },
            ordered = TRUE
          ),

          # Add tournament name, category, date
          tournament_name = t_name,
          category = t_category,
          tournament_date = t_date
        ) %>%
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
        ) %>%
        mutate(match = nrow(.) - row_number()) ## generate match number

      ## Clean results to get game level data
      result <- result %>%
        mutate(games = strsplit(as.character(games), ", ")) %>% ## separate games into list elements
        unnest(games) %>% ## unnest list elements into rows
        group_by(tournament_name, round, player_1, player_2, match_time) %>%
        mutate(game = row_number()) %>%  ## generate game number
        ungroup() %>%
        mutate(
          player_1_score = as.numeric(str_extract(games, pattern = "^[:digit:]+(?=\\-)")),
          ## Extract player 1's score
          player_2_score = as.numeric(str_extract(games, pattern = "(?<=\\-)[:digit:]+")),
          ## Extract player 2's score
          game_winner = if_else(player_1_score > player_2_score, player_1, player_2)
        ) %>%  ## Extract game winner based on player scores
        select(-match_winner,-match_time,-games)

      ## Bind results row-wise
      games <- rbind(games, result)

    }

    ## Organize results
    games <- games %>%
      select(
        tournament_name,
        category,
        tournament_date,
        round,
        match,
        game,
        player_1,
        player_2,
        game_winner,
        player_1_score,
        player_2_score,
        player_1_seed,
        player_2_seed,
        player_1_nationality,
        player_2_nationality
      ) %>%
      arrange(tournament_date, desc(round), desc(match), desc(game))


    return(games)


  }
