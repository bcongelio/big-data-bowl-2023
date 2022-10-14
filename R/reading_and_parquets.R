library(tidyverse)
library(arrow)

### reading all weeks and writing to a parquet file
all_bdb_weeks <- function(dir = file.path('core-data')) {
  paths <- fs::dir_ls(dir, regexp = 'week\\d+')
  all_weeks <-
    paths %>%
    purrr::map_df(vroom::vroom) %>%
    janitor::clean_names() %>%
    arrow::write_parquet(file.path('core-data', 'data.parquet'))
}

all_bdb_weeks()


### reading in all play information provided and writing to a parquet file
read_bdb_plays <- memoise::memoise({function() {
  plays <- file.path('core-data', 'plays.csv') %>%
    readr::read_csv() %>%
    janitor::clean_names() %>%
    arrow::write_parquet(file.path('core-data', 'plays.parquet'))
}})

read_bdb_plays()

### reading in individual game information and writing to a parquet file
read_game_info <- memoise::memoise({function() {
  file.path('core-data', 'games.csv') %>%
    readr::read_csv() %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(game_date = lubridate::mdy)) %>%
    arrow::write_parquet(file.path('core-data', 'games.parquet'))
}})


### creating an individual .cvs file for each game in bdb
all.weeks <- read_parquet("./core-data/large-lfs-files/all-weeks-parquet")

all.weeks %>%
  group_by(game_id) %>%
  group_walk(~ write_csv(.x, paste0('tracking_gameId_', .y$game_id, ".csv")))