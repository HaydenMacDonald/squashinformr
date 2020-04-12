
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

### `get_player_rankings_history()`

This function returns player ranking histories, given their full names
or current PSA rankings and competition category.

Let’s contrast the ranking histories of the Mohamed Elshorbagy (Men’s
Rank 1 in April 2020) and Ali Farag (Men’s Rank 2 in April 2020)…

``` r
library(squashinformr)
library(ggplot2)

## Get the rankings history for the top two men's singles players
top_two <- get_player_rankings_history(player = c("Mohamed Elshorbagy", "Ali Farag"), category = "mens")

## Plot
ggplot(top_two) +
   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
   scale_y_reverse() +
   theme_light() +
   labs(x = "Date", y = "Rank")
# Warning: Removed 28 row(s) containing missing values (geom_path).
```

<img src="man/figures/unnamed-chunk-2-1.png" width="100%" />

Let’s contrast the ranking histories of the current top 3 Women’s
players…

``` r
library(squashinformr)
library(ggplot2)

## Get the rankings history for the top three women's singles players
top_three <- get_player_rankings_history(rank = 1:3, category = "womens")

ggplot(top_three) +
   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
   scale_y_reverse() +
   theme_light() +
   labs(x = "Date", y = "Rank")
# Warning: Removed 34 row(s) containing missing values (geom_path).
```

<img src="man/figures/unnamed-chunk-3-1.png" width="100%" />

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
