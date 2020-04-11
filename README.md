# squashinformr

[![Travis build status](https://travis-ci.org/HaydenMacDonald/squashinformr.svg?branch=master)](https://travis-ci.org/github/HaydenMacDonald/squashinformr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/HaydenMacDonald/squashinformr?branch=master&svg=true)](https://ci.appveyor.com/project/HaydenMacDonald/squashinformr)

Politely web scrape data from SquashInfo in R

## Installation

Install the development version of `squashinformr` from this GitHub repository via:

```{r}
if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("HaydenMacDonald/squashinformr")
```

## Usage

There are three major families of scraping functions in `squashinformr`:

1. Player functions for scraping player profile data
2. Tournament functions for scraping tournament results data
3. Ranking functions for scraping current and historical rankings

## Examples

### get_player_rankings_history()

```{r}
library(squashinformr)

## Get the rankings history for the top two men's singles players
top_two <- get_player_rankings_history(rank = 1:2, category = "mens")

ggplot(top_two) +
   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
   scale_y_reverse()
```
<>


```{r}
library(squashinformr)

## Get the rankings history for the top three women's singles players
top_three <- get_player_rankings_history(rank = 1:3, category = "womens")

ggplot(top_three) +
   geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
   scale_y_reverse()
```

<>


## Help

Submit issues here on GitHub.  

If you are interested in extending the functionality of this package, fork this repository, make your changes and submit them as a pull request.  

## Disclaimer

SquashInfo is a valuable resource for the international squash community. By creating and sharing this package, I do not intend to compete with SquashInfo or any of its stakeholders. The `squashinformr` package was created to allow individuals to access data from SquashInfo in an efficient and responsible way, using [`polite` principles](https://github.com/dmi3kno/polite). Following `polite` principles incurs mandatory delays on the scraping process set by SquashInfo. This prevents the use of this package from incurring unnecessary harm to SquashInfo servers via overwhelming requests. Therefore, it is important that users of this package are patient when scraping and respectful of SquashInfo and the work they produce.  

## Author

This package was authored by Hayden MacDonald (hayden.macdonald.8778@gmail.com). 

## License

The `squashinformr` package is licensed under the [MIT LICENSE](https://github.com/HaydenMacDonald/squashinformr/blob/master/LICENSE).
