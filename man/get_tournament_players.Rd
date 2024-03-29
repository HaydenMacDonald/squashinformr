% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tournament_.R
\name{get_tournament_players}
\alias{get_tournament_players}
\title{Get a tournament's players from SquashInfo}
\usage{
get_tournament_players(tournament = NULL, year = 2022, world_tour = TRUE)
}
\arguments{
\item{tournament}{character string of name of the tournament. Partial tournament names are matched via regular expressions.}

\item{year}{integer indicating year of competition. Must be 2021, 2022, or NULL if querying results for both years.}

\item{world_tour}{logical indicating whether to only return PSA World Tour tournaments.}
}
\value{
Tibble containing the tournament name, competition category, tournament date, player, seed, nationality, round_reached.
}
\description{
Given a tournament name or a year, \code{get_tournament_players()} returns player registrants for PSA World Tour tournaments and other events.
}
\note{
This function only returns player registrant data from tournaments in 2021 and 2022, as any other data are not available to non-premium members on SquashInfo. Additionally, events that do not use a single elimination format are not included in the results (e.g. Karakal Premier League).
}
\examples{
## Who played in the Allam British Open in 2022?
\donttest{get_tournament_players(tournament = "Allam British Open", year = 2022)}

## Return player registrant data for all PSA World Tour tournaments in 2022
\donttest{get_tournament_players(year = 2022, world_tour = TRUE)}

}
\references{
\url{http://www.squashinfo.com/results}
}
