pit.buff <- arrow::read_parquet("./core-data/large-lfs-files/all-weeks-parquet") %>%
  filter(game_id == "2021091201")

game.plays <- read_bdb_plays() %>%
  filter(game_id == "2021091201")

all.merged <- inner_join(pit.buff, game.plays,
                         by = c("play_id" = "play_id"))

