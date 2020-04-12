
# squashinformr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/HaydenMacDonald/squashinformr.svg?branch=master)](https://travis-ci.org/github/HaydenMacDonald/squashinformr)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/HaydenMacDonald/squashinformr?branch=master&svg=true)](https://ci.appveyor.com/project/HaydenMacDonald/squashinformr)
<!-- badges: end -->

Politely web scrape data from
<a href="http://www.squashinfo.com/" target="_blank">SquashInfo</a> in R

## Installation

Install the development version of `squashinformr` from this GitHub
repository via:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("HaydenMacDonald/squashinformr")
```

## Usage

There are three major families of scraping functions in `squashinformr`:

1.  Player functions (`get_players()`, `get_player_*()`) for scraping
    player profile data
2.  Tournament functions (`get_tournaments()`, `get_tournament_*()`) for
    scraping tournament results data
3.  Ranking functions (`get_rankings()` & `get_historical_rankings()`)
    for scraping current and historical rankings tables

## Examples

### `get_player_recent_matches()`

This function returns a player’s recent matches.

``` r
library(squashinformr)
library(ggplot2)

## Get Mohamed Elshorbagy's most recent match data
get_player_recent_matches(player = "Mohamed Elshorbagy", category = "mens")
# Scraping http://www.squashinfo.com/rankings/men/1
# Scraping Mohamed Elshorbagy's profile
# # A tibble: 75 x 12
#     rank player             opponent          result games_won games_lost match_time round date       event                    country psa  
#    <int> <chr>              <chr>             <chr>      <dbl>      <dbl>      <dbl> <chr> <date>     <chr>                    <chr>   <chr>
#  1     1 Mohamed Elshorbagy Ali Farag         W              3          1         79 F     2020-03-01 Canary Wharf Classic     ENG     Y    
#  2     1 Mohamed Elshorbagy Tarek Momen       W              3          1         49 SF    2020-03-01 Canary Wharf Classic     ENG     Y    
#  3     1 Mohamed Elshorbagy Saurav Ghosal     W              2          1         51 QF    2020-03-01 Canary Wharf Classic     ENG     Y    
#  4     1 Mohamed Elshorbagy Mathieu Castagnet W              2          1         48 R2    2020-03-01 Canary Wharf Classic     ENG     Y    
#  5     1 Mohamed Elshorbagy Paul Coll         L              0          3         51 QF    2020-03-01 Windy City Open          USA     Y    
#  6     1 Mohamed Elshorbagy Mazen Hesham      W             NA         NA         NA R3    2020-03-01 Windy City Open          USA     Y    
#  7     1 Mohamed Elshorbagy Daryl Selby       W              3          0         30 R2    2020-03-01 Windy City Open          USA     Y    
#  8     1 Mohamed Elshorbagy Paul Coll         L              1          2         NA -     2020-02-01 Premier League 8th Round ENG     N    
#  9     1 Mohamed Elshorbagy Peter Creed       W              2          0         NA -     2020-01-01 Premier League 7th Round ENG     N    
# 10     1 Mohamed Elshorbagy Tarek Momen       W              3          1         76 F     2020-01-01 Tournament of Champions  USA     Y    
# # ... with 65 more rows
```

### `get_player_rankings_history()`

This function returns player ranking histories, given their full names
or current PSA rankings and competition category.

``` r
library(squashinformr)
library(ggplot2)

## Get the rankings history for the top three women's singles players
top_three <- get_player_rankings_history(rank = 1:3, category = "womens")
# Scraping Raneem El Welily's ranking history
# Scraping Nouran Gohar's ranking history
# Scraping Nour El Sherbini's ranking history

ggplot(top_three) +
   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
   scale_y_reverse() +
   theme_light() +
   labs(x = "Date", y = "Rank")
# Warning: Removed 34 row(s) containing missing values (geom_path).
```

<img src="man/figures/unnamed-chunk-3-1.png" width="100%" />

### `get_tournament_games()`

This function returns a tournament’s game results data.

``` r
library(squashinformr)
library(ggplot2)

## Return game data for 2020's Tournament of Champions.
get_tournament_games("tournament of champions", year = 2020)
# Scraping http://www.squashinfo.com/results?start=1
# Scraping http://www.squashinfo.com/results?start=2
# Scraping http://www.squashinfo.com/results?start=3
# Scraping http://www.squashinfo.com/results?start=4
# Scraping http://www.squashinfo.com/results?start=5
# Scraping http://www.squashinfo.com/events/8367-mens-tournament-of-champions-2020
# Scraping http://www.squashinfo.com/events/8368-womens-tournament-of-champions-2020
# # A tibble: 388 x 15
#    tournament_name            category tournament_date round match  game player_1       player_2      game_winner    player_1_score player_2_score player_1_seed player_2_seed player_1_national~ player_2_national~
#    <chr>                      <chr>    <date>          <ord> <int> <int> <chr>          <chr>         <chr>                   <dbl>          <dbl>         <dbl>         <dbl> <chr>              <chr>             
#  1 JP Morgan Tournament of C~ Men's    2020-01-17      F        64     4 Mohamed Elsho~ Tarek Momen   Mohamed Elsho~             11              5             2             4 EGY                EGY               
#  2 JP Morgan Tournament of C~ Men's    2020-01-17      F        64     3 Mohamed Elsho~ Tarek Momen   Mohamed Elsho~             11              7             2             4 EGY                EGY               
#  3 JP Morgan Tournament of C~ Men's    2020-01-17      F        64     2 Mohamed Elsho~ Tarek Momen   Mohamed Elsho~             11              7             2             4 EGY                EGY               
#  4 JP Morgan Tournament of C~ Men's    2020-01-17      F        64     1 Mohamed Elsho~ Tarek Momen   Tarek Momen                 9             11             2             4 EGY                EGY               
#  5 JP Morgan Tournament of C~ Women's  2020-01-17      F        62     3 Camille Serme  Nour El Sher~ Camille Serme              11              7             5             2 FRA                EGY               
#  6 JP Morgan Tournament of C~ Women's  2020-01-17      F        62     2 Camille Serme  Nour El Sher~ Camille Serme              11              6             5             2 FRA                EGY               
#  7 JP Morgan Tournament of C~ Women's  2020-01-17      F        62     1 Camille Serme  Nour El Sher~ Camille Serme              11              8             5             2 FRA                EGY               
#  8 JP Morgan Tournament of C~ Men's    2020-01-17      SF       63     5 Tarek Momen    Ali Farag     Tarek Momen                11              7             4             1 EGY                EGY               
#  9 JP Morgan Tournament of C~ Men's    2020-01-17      SF       63     4 Tarek Momen    Ali Farag     Ali Farag                   8             11             4             1 EGY                EGY               
# 10 JP Morgan Tournament of C~ Men's    2020-01-17      SF       63     3 Tarek Momen    Ali Farag     Ali Farag                   7             11             4             1 EGY                EGY               
# # ... with 378 more rows
```

### `get_rankings()`

This function returns data from the most recent PSA rankings tables.

``` r
library(squashinformr)
library(ggplot2)
library(dplyr)

## Get the top 5 players in both men's and women's singles competitions
get_rankings(top = 5, category = "both") %>%
    arrange(category)
# # A tibble: 10 x 8
#     rank previous_rank name               highest_world_ranking hwr_date   country category highest_ranking
#    <int>         <int> <chr>                              <int> <date>     <chr>   <chr>              <int>
#  1     1             1 Mohamed Elshorbagy                     1 2014-11-01 EGY     Men's                 NA
#  2     2             2 Ali Farag                              1 2019-03-01 EGY     Men's                 NA
#  3     3             4 Karim Abdel Gawad                      1 2017-05-01 EGY     Men's                 NA
#  4     4             3 Tarek Momen                            3 2019-02-01 EGY     Men's                 NA
#  5     5             5 Paul Coll                              5 2019-04-01 NZL     Men's                 NA
#  6     1             1 Raneem El Welily                      NA 2015-09-01 EGY     Women's                1
#  7     2             2 Nouran Gohar                          NA 2017-01-01 EGY     Women's                2
#  8     3             4 Nour El Sherbini                      NA 2016-05-01 EGY     Women's                1
#  9     4             3 Camille Serme                         NA 2017-02-01 FRA     Women's                2
# 10     5             5 Nour El Tayeb                         NA 2018-02-01 EGY     Women's                3
```

## Help

Submit issues here on GitHub.

If you are interested in extending the functionality of this package,
fork this repository, make your changes and submit them as a pull
request. The `squashinformr` project is released with a
<a href="https://github.com/HaydenMacDonald/squashinformr/blob/master/CODE_OF_CONDUCT.md" target="_blank">Contributor
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
overwhelming requests. Therefore, it is important that users of this
package are patient when scraping and respectful of SquashInfo and the
work they produce.

## Author

This package was authored by Hayden MacDonald
(<hayden.macdonald.8778@gmail.com>).

## License

The `squashinformr` package is licensed under the
<a href="https://github.com/HaydenMacDonald/squashinformr/blob/master/LICENSE" target="_blank">MIT
LICENSE</a>.
