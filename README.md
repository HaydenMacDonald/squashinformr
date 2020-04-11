# squashinformr

[![Travis build status](https://travis-ci.org/HaydenMacDonald/squashinformr.svg?branch=master)](https://travis-ci.org/github/HaydenMacDonald/squashinformr)

Politely web scrape data from SquashInfo in R

## Installation

```{r}
if (!requireNamespace("remotes")) install.packages("remotes")

remotes::install_github("HaydenMacDonald/hmdrmd")
```

## Usage

There are two major families of scraping functions in `squashinformr`:

1. `get_player_*` for scraping player profile data
2. `get_tournament_*` for scraping tournament results data

## Examples

```{r}
## Get the rankings history for the top two men's singles players
   top_two <- get_player_rankings_history(rank = 1:2, category = "mens")

   ggplot(top_two) +
    geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
    scale_y_reverse()
```


```{r}
## Get the rankings history for the top three women's singles players
   top_three <- get_player_rankings_history(rank = 1:3, category = "womens")

   ggplot(top_three) +
     geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
     scale_y_reverse()
```
## Help

Submit issues here on GitHub.  

If you are interested in extending the functionality of this package, fork this repository, make your changes and submit them as a pull request.  

## Disclaimer

SquashInfo is a valuable resource for the international squash community. It has never been my intention to compete with SquashInfo. By creating and sharing this package, I do not intend to compete with SquashInfo or any of its stakeholders. The `squashinformr` package has been created to allow individuals to access data from SquashInfo in a efficient and responsible way, using [`polite` principles](https://github.com/dmi3kno/polite). Following `polite` principles incurs mandatory delays on the scraping process set out by SquashInfo. This prevents the use of this package from incurring unnecessary harm to SquashInfo servers through overwhelming requests. Therefore, it is important that users of this package are patient and respectful of SquashInfo and the work they produce.  

## Author

This package was authored by Hayden MacDonald (hayden.macdonald.8778@gmail.com). 

## License

The `squashinformr` package is licensed under the [MIT LICENSE](https://github.com/HaydenMacDonald/squashinformr/blob/master/LICENSE).
