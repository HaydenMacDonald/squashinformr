
# squashinformr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/squashinformr)](https://CRAN.R-project.org/package=squashinformr)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/squashinformr)](https://cranlogs.r-pkg.org/badges/squashinformr)
[![CRAN total](https://cranlogs.r-pkg.org/badges/grand-total/squashinformr)](https://cranlogs.r-pkg.org/badges/grand-total/squashinformr)  

[![R build status](https://github.com/HaydenMacDonald/squashinformr/workflows/R-CMD-check/badge.svg)](https://github.com/HaydenMacDonald/squashinformr/actions)
[![Codecov test coverage](https://codecov.io/gh/HaydenMacDonald/squashinformr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/HaydenMacDonald/squashinformr)
[![CodeFactor](https://www.codefactor.io/repository/github/haydenmacdonald/squashinformr/badge)](https://www.codefactor.io/repository/github/haydenmacdonald/squashinformr)
[![Tutorial](https://img.shields.io/badge/-Tutorial%20Blog%20Post-blue?style=flat&logo=RSS&logoColor=white)](https://needleinthehay.ca/post/introducing-squashinformr/)
<!-- badges: end -->

## Overview

**squashinformr** allows users to easily scrape
<a href="http://www.squashinfo.com/" target="_blank">SquashInfo</a> for
data on the Professional Squash Association World Tour and other squash
tournaments. The functions within this package scrape, parse, and clean
data associated with players, tournaments, and rankings.

## Installation

Install `squashinformr` via CRAN:

``` r
install.packages("squashinformr")
```

Install the development version of `squashinformr` from this GitHub
repository via:

``` r
if (!require("remotes")) install.packages("remotes")

remotes::install_github("HaydenMacDonald/squashinformr")
```

## Usage

There are three major families of scraping functions in `squashinformr`:

  - Player functions for scraping player profile data
      - `get_players()`
      - `get_player_recent_results()`
      - `get_player_recent_matches()`
      - `get_player_recent_games()`
      - `get_matchup()`
  - Tournament functions for scraping tournament results data
      - `get_tournaments()`
      - `get_tournament_players()`
      - `get_tournament_matches()`
      - `get_tournament_games()`
  - Ranking function for scraping current and historical rankings tables
      - `get_rankings()`

## Examples

### `get_player_recent_matches()`

This function returns a player’s recent matches.

``` r
library(squashinformr)

## Get Mohamed Elshorbagy's most recent match data
get_player_recent_matches(player = "Mohamed Elshorbagy", category = "mens")
#> Scraping http://www.squashinfo.com/rankings/men/1
#> Scraping Mohamed Elshorbagy's recent results
#> # A tibble: 10 x 12
#>     rank player opponent result games_won games_lost match_time round date      
#>    <int> <chr>  <chr>    <chr>      <dbl>      <dbl>      <dbl> <chr> <date>    
#>  1     2 Moham~ Mostafa~ L             NA         NA         NA F     2021-02-01
#>  2     2 Moham~ Lucas S~ W              3          2         NA SF    2021-02-01
#>  3     2 Moham~ Dimitri~ W              3          1         NA QF    2021-02-01
#>  4     2 Moham~ Youssef~ L              2          3         65 R3    2020-11-01
#>  5     2 Moham~ George ~ W              3          1         61 R2    2020-11-01
#>  6     2 Moham~ Karim A~ W              3          1         73 F     2020-09-01
#>  7     2 Moham~ Paul Co~ W              3          2         84 SF    2020-09-01
#>  8     2 Moham~ Joel Ma~ W              3          1         69 QF    2020-09-01
#>  9     2 Moham~ James W~ W              3          0         36 R2    2020-09-01
#> 10     2 Moham~ Raphael~ W              3          0         34 R1    2020-09-01
#> # ... with 3 more variables: event <chr>, country <chr>, psa <chr>
```

### `get_tournament_games()`

This function returns a tournament’s game results data.

``` r
## Return game data for 2020's Black Ball Open.
get_tournament_games("Black Ball Open", year = 2020, world_tour = TRUE)
#> Scraping http://www.squashinfo.com/results?start=1
#> Scraping http://www.squashinfo.com/results?start=2
#> Scraping http://www.squashinfo.com/results?start=3
#> Scraping http://www.squashinfo.com/events/8425-mens-black-ball-open-2020
#> # A tibble: 119 x 15
#>    tournament_name category tournament_date player_1 player_2 player_1_seed
#>    <chr>           <chr>    <date>          <chr>    <chr>            <dbl>
#>  1 CIB Black Ball~ Men's    2020-12-18      Fares D~ Ali Far~             9
#>  2 CIB Black Ball~ Men's    2020-12-18      Fares D~ Ali Far~             9
#>  3 CIB Black Ball~ Men's    2020-12-18      Fares D~ Ali Far~             9
#>  4 CIB Black Ball~ Men's    2020-12-18      Fares D~ Ali Far~             9
#>  5 CIB Black Ball~ Men's    2020-12-18      Fares D~ Ali Far~             9
#>  6 CIB Black Ball~ Men's    2020-12-18      Ali Far~ Mostafa~             1
#>  7 CIB Black Ball~ Men's    2020-12-18      Ali Far~ Mostafa~             1
#>  8 CIB Black Ball~ Men's    2020-12-18      Ali Far~ Mostafa~             1
#>  9 CIB Black Ball~ Men's    2020-12-18      Fares D~ Tarek M~             9
#> 10 CIB Black Ball~ Men's    2020-12-18      Fares D~ Tarek M~             9
#> # ... with 109 more rows, and 9 more variables: player_2_seed <dbl>,
#> #   player_1_nationality <chr>, player_2_nationality <chr>, round <ord>,
#> #   match <int>, game <int>, player_1_score <dbl>, player_2_score <dbl>,
#> #   game_winner <chr>
```

### `get_rankings()`

This function returns data from the most recent PSA rankings tables.

``` r
library(dplyr)

## Get the top 5 players in both men's and women's singles competitions
get_rankings(top = 5, category = "both") %>%
    arrange(category, rank)
#> # A tibble: 10 x 7
#>     rank previous_rank name        highest_world_ra~ hwr_date   country category
#>    <int>         <int> <chr>                   <int> <date>     <chr>   <chr>   
#>  1     1             1 Ali Farag                   1 2019-03-01 EGY     mens    
#>  2     2             2 Mohamed El~                 1 2014-11-01 EGY     mens    
#>  3     3             3 Tarek Momen                 3 2019-02-01 EGY     mens    
#>  4     4             4 Paul Coll                   4 2020-12-01 NZL     mens    
#>  5     5             5 Karim Abde~                 1 2017-05-01 EGY     mens    
#>  6     1             1 Nour El Sh~                 1 2016-05-01 EGY     womens  
#>  7     2             2 Nouran Goh~                 1 2020-07-01 EGY     womens  
#>  8     3             3 Camille Se~                 2 2017-02-01 FRA     womens  
#>  9     4             4 Nour El Ta~                 3 2018-02-01 EGY     womens  
#> 10     5             5 Hania El H~                 5 2020-11-01 EGY     womens
```

### `get_matchup()`

This function returns recent head-to-head matchup stats between two
players. Stats returned include:

  - each player’s rank and name
  - total matches played
  - the number of matches won
  - the match results spread (relative to player 1)
  - the average match time
  - the number of games played
  - the number of games won
  - average point advantage in a won game
  - the average point difference in final scores
  - the number of tie-break wins
  - and the percentage of games that go to a tie-breaker.

<!-- end list -->

``` r
## Get tidy matchup stats for Paul Coll vs Fares Dessouky
get_matchup("Paul Coll", "Fares Dessouky", category = "mens", tidy = FALSE)
#> # A tibble: 23 x 2
#>    metric               value         
#>    <chr>                <chr>         
#>  1 player_1_rank        4             
#>  2 player_1             Paul Coll     
#>  3 player_2_rank        8             
#>  4 player_2             Fares Dessouky
#>  5 matches_played       2             
#>  6 player_1_matches_won 2             
#>  7 player_2_matches_won 0             
#>  8 matches_3_2          0             
#>  9 matches_3_1          1             
#> 10 matches_3_0          1             
#> # ... with 13 more rows
```

## Help

Submit issues here on GitHub.

If you are interested in extending the functionality of this package,
fork this repository, make your changes and submit them as a pull
request. The `squashinformr` project is released with a
<a href="https://github.com/HaydenMacDonald/squashinformr/blob/main/.github/CODE_OF_CONDUCT.md" target="_blank">Contributor
Code of Conduct</a>. By contributing to this project, you agree to its
terms.

## Disclaimer

SquashInfo is a valuable resource for the international squash
community. By creating and sharing this package, I do not intend to
compete with SquashInfo or any of its stakeholders. The `squashinformr`
package was created to allow individuals to access data from SquashInfo
in an efficient and responsible way, using
<a href="https://github.com/dmi3kno/polite" target="_blank">`polite`
principles</a>. Following `polite` principles incurs mandatory delays on
the scraping process set by SquashInfo. This prevents the use of this
package from incurring unnecessary harm to SquashInfo servers via
overwhelming requests. Therefore, it is important that users are patient
when using this package. SquashInfo currently offers full access to
their data and extra features through a premium membership. Please
consider
<a href="http://www.squashinfo.com/subscriptions" target="_blank">signing up
and subscribing</a> to SquashInfo to support their work.

## Author

This package was authored by Hayden MacDonald. Feel free to email me at
hmd\[at\]needleinthehay.ca.

## License

The `squashinformr` package is released under a
<a href="https://github.com/HaydenMacDonald/squashinformr/blob/main/LICENSE.md" target="_blank">GPL-3</a>
license.
