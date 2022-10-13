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

