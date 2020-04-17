#' Get a tournament's players from SquashInfo
#'
#' Given a tournament name or a year, \code{get_tournament_players()} returns player registrants for PSA World Tour tournaments and other events.
#'
#' @param tournament character string of name of the tournament. Partial tournament names are matched via regular expressions.
#'
#' @param year integer indicating year of competition. Must be 2019 or 2020.
#'
#' @param world_tour logical indicating whether to only return PSA World Tour tournaments.
#'
#' @return Tibble containing the tournament name, competition category, tournament date, player, seed, nationality, round_reached.
#'
#' @examples
#' ## Who played in the Tournament of Champions in 2019?
#' get_tournament_players(tournament = "tournament of champions", year = 2019)
#'
#' ## Return player registrant data for all PSA World Tour tournaments in 2019
#' \donttest{get_tournament_players(year = 2019, world_tour = TRUE)}
#'
#'
#'
#' @note This function only returns player registrant data from tournaments in 2019 and 2020, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
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
#' @importFrom dplyr funs_
#' @importFrom dplyr desc
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
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

get_tournament_players <- function(tournament = NULL, year = 2020, world_tour = TRUE) {

  ## Input errors
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

    ## Extract tournament slugs
    slug <- current_page %>%
              html_nodes(xpath = "//td/a") %>%
              html_attr("href") %>%
              .[str_detect(., "events")] %>%
              .[!str_detect(., "premier-league")]

    ## Add slugs to table
    results <- cbind(results, slug)

    ## Filter out non-world tour tournaments
    if (world_tour == TRUE) {

      results <- results %>%
                    filter(str_detect(league, pattern = "World"))

    }

    ## Bind results row-wise
    tournaments <- rbind(tournaments, results)


    ## If tournament name is given and year is not given
    if (is.null(tournament) == FALSE & is.null(year) == TRUE) {

          ## and tournament name is present within current results
          if (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE))) {

            results_limit <- NA_character_ ## end while loop

          } else { ## otherwise continue to next page

            results_limit <- 0

            # Find url in "Next" button
            t_url <- current_page %>%
                        html_nodes("a")

            t_url <- suppressWarnings(t_url[str_detect(t_url, "Next")]) %>%
                        html_attr("href")

            ## Replace t_url with url in Next page button
            t_url <- sprintf("http://www.squashinfo.com%s", t_url)

          }

    ## If tournament name is not given and year is given
    } else if (is.null(tournament) == TRUE & is.null(year) == FALSE) {

          ## and the previous year is present within current results
          if ((year - 1) %in% year(tournaments$date)) {

            results_limit <- NA_character_ ## end while loop

          } else { ## otherwise continue to next page

            results_limit <- 0

            # Find url in "Next" button
            t_url <- current_page %>%
                        html_nodes("a")

            t_url <- suppressWarnings(t_url[str_detect(t_url, "Next")]) %>%
                        html_attr("href")

            ## Replace t_url with url in Next page button
            t_url <- sprintf("http://www.squashinfo.com%s", t_url)

          }

    ## If tournament AND year are given
    } else {

        ## AND if tournament name and year given are present in current results
        if ((year %in% year(tournaments[str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)), ]$date)) & (TRUE %in% str_detect(tournaments$name, regex(tournament, ignore_case = TRUE)))) {

          results_limit <- NA_character_ ## end while loop

        } else { ## otherwise continue to next page

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

  } # END OF WHILE LOOP


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

  ## Create empty players dataframe
  players <- c()

  for (i in 1:length(tournaments$slug)) { ## for each slug in tournaments

    ## Create url from slug
    t_url <- sprintf("http://www.squashinfo.com%s", tournaments$slug[i])

    ## Verbose
    message("Scraping ", t_url)

    ## Extract tournament name
    t_name <- tournaments$name[i]

    ## Extract tournament category
    t_category <- if_else(str_detect(t_name, pattern = regex(" \\(M\\)")), "Men's", if_else(str_detect(t_name, pattern = regex(" \\(W\\)")), "Women's", NA_character_))

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

    ## Filter out extraneous rows from table
    result <- result[rowSums(is.na(result)) != ncol(result),]


    ## Create dataframe of lags for each match in order to determine which tournament round the match occurred in
    lags <- seq(1, dim(result)[1], 1)

    lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")

    lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

    lag_df <- result %>%
                mutate_at(vars(X1), suppressWarnings(funs_(lag_functions)))

    ## Create round variable
    result$round <- NA

    ## For each match in result, find the corresponding lagged 'round' row
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

    ## Clean results
    result <- result %>%
                ## Remove 'round' rows
                filter(X1 != "Final:", X1 != "Semi-finals:", X1 != "Quarter-finals:", X1 != "3rd round:", X1 != "2nd round:", X1 != "1st round:") %>%
                mutate(X1 = str_replace_all(X1, pattern = regex("[:punct:]"), replacement = ""), ## Remove punctuation from match column

                       ## Extract player names
                       player_2 = str_trim(if_else(X2 == "bye", "bye", if_else(str_detect(X1, " v "), str_extract(X1, pattern = "(?<= v).*"), str_extract(X1, pattern = "(?<= bt).*"))), side = "both"),
                       player_1 = str_trim(if_else(X2 == "bye", X1, if_else(str_detect(X1, " v "), str_extract(X1, pattern = ".*(?= v)"), str_extract(X1, pattern = ".*(?= bt)"))), side = "both"),

                       ## Extract player seed
                       player_1_seed = as.numeric(str_extract(player_1, pattern = regex("^[:digit:]{1,}"))),
                       player_2_seed = as.numeric(str_extract(player_2, pattern = regex("^[:digit:]{1,}"))),

                       ## Remove seed data from player names
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
                                      ordered = TRUE)) %>%
                select(player_1, player_2, player_1_seed, player_2_seed, player_1_nationality, player_2_nationality, round)

    ## Extract player_1 data in each match
    player_1 <- result %>%
                    select(player = player_1, player_seed = player_1_seed, player_nationality = player_1_nationality, round)

    ## Extract player_2 data in each match
    player_2 <- result %>%
                    select(player = player_2, player_seed = player_2_seed, player_nationality = player_2_nationality, round)

    ## Bind results row-wise and find unique players in tournament
    result <- bind_rows(player_1, player_2) %>%
                    group_by(player) %>%
                    summarize(seed = suppressWarnings(max(player_seed, na.rm = TRUE)),
                              nationality = suppressWarnings(max(player_nationality, na.rm = TRUE)),
                              round_reached = suppressWarnings(max(round, na.rm = TRUE))) %>%  ## find highest round reached by player in that tournament
                    filter(player != "bye") %>%
                    ungroup() %>%
                    mutate(seed = if_else(is.infinite(seed), NA_real_, if_else(seed == 916, 16, seed))) %>% ## Give seed 16 to player's given seed '9-16'
                    arrange(seed)

    ## Add tournament name, category, date
    result <- result %>%
                mutate(tournament = t_name,
                       category = t_category,
                       tournament_date = t_date) %>%
                select(tournament, category, tournament_date, player, seed, nationality, round_reached)

    ## Bind results row-wise
    players <- rbind(players, result)


  }


  return(players)


}

