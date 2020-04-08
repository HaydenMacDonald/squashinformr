# squashinformr
Politely web scrape data from SquashInfo in R

```{r}
## Get the rankings history for the top two men's singles players
   top_two <- get_player_rankings_history(rank = 1:2, category = "mens")

   ggplot(top_two) +
    geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
    scale_y_reverse()

## Get the rankings history for the top three women's singles players
   top_three <- get_player_rankings_history(rank = 1:3, category = "womens")

   ggplot(top_three) +
     geom_line(aes(x = exact_date, y = rank, group = name, colour = name)) +
     scale_y_reverse()
```
