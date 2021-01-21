# squashinformr 0.2.0

## Package Refactor

- Reduce susceptibility to bugs
- Increase package maintainability
- Increase code readability
- Make the package easier to contribute to

## Major changes

- Refactored R functions to reduce the amount of repeated code via helper functions.
- Reorganized functions into files by family where applicable.
- Updated R version and package dependency versions.
- Updated tests and documentation according to refactored code.



# squashinformr 0.1.0

## Major Changes

- Added News.md for tracking changes
- Initial player functions for scraping player profile data
  - `get_players()`
  - `get_player_recent_results()`
  - `get_player_recent_matches()`
  - `get_player_recent_games()`
  - `get_player_rankings_history()`
  - `get_matchup()`
- Initial tournament functions for scraping tournament results data
  - `get_tournaments()`
  - `get_tournament_players()`
  - `get_tournament_matches()`
  - `get_tournament_games()`
- Initial ranking functions for scraping current and historical rankings tables
  - `get_rankings()`
  - `get_historical_rankings()`
