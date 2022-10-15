library(tidyverse)
library(arrow)

source("R/gg_field.R")

########
## READING IN PITTSBURGH VS. BUFFALO - WEEK 1
########

pitt.buff <- arrow::read_parquet("core-data/large-lfs-files/all-weeks-parquet") %>%
  filter(game_id == "2021091201")


########
## READING IN PLAYS FROM PITTSBURGH VS. BUFFALO - WEEK 1
########

pitt.buff.plays <- readr::read_csv("core-data/plays.csv") %>%
  janitor::clean_names() %>%
  filter(game_id == "2021091201")


########
## READING IN GAME INFO FROM PITTSBURGH VS. BUFFALO - WEEK 1
########

pitt.buff.info <- readr::read_csv("core-data/games.csv") %>%
  janitor::clean_names() %>%
  filter(game_id == "2021091201")

########
## COMBING CORE GAME FILE WITH PLAYS, AND THEN BY INFO (TO AVOID MULTIPLE GAME_IDs IN DF)
########

complete.data <- inner_join(pitt.buff, pitt.buff.plays, by = c("game_id" = "game_id", "play_id" = "play_id"))

complete.data <- complete.data %>%
  inner_join(pitt.buff.info, by = c("game_id" = "game_id"))

### ROTATING THE DOTS
complete.data <- rotate_the_dots(complete.data)

########
## MUTATING TO CHARACTER VARIABLE DEFINING WHETHER TEAM IN FRAMES IS ON OFFENSE OR DEFENSE
########

complete.data <- complete.data %>%
  mutate(off_or_def = case_when(
    team == possession_team ~ "offense",
    team != possession_team ~ "defense",
    TRUE ~ "football"))

            ########
            ## CORE CLEANING AND PREP IS COMPLETE
            ########

########
## ADDING IN INFORMATION FROM PLAYERS.CSV TO BUILD CHULLs FOR JUST O-LINE
########

player.info <- readr::read_csv("core-data/players.csv") %>%
  janitor::clean_names() %>%
  select(nfl_id, official_position, display_name)

complete.data <- complete.data %>%
  inner_join(player.info, by = c("nfl_id" = "nfl_id"))

########
## LET'S PICK OUT A FUN PLAY TO WORK WITH
########

one.play <- complete.data %>%  ### BEN PASS TO EBRON FOR 19 YARDS
  filter(play_id == 2209)

########
## NOW LET'S BUILD A CONVEX HULL FOR JUST THE OFFENSIVE LINE
########

ol_chull_order <- one.play %>%
  filter(off_or_def == "offense") %>%
  filter(official_position %in% c("T", "C", "G")) %>%  #### IMPORTANT TO KNOW PERSONNEL PACKAGE HERE: 0 RB, 0 TE, 5 WR
  select(frame_id, x, y) %>%
  chull

ol_chull_order <- c(ol_chull_order, ol_chull_order[1])

ol_chull_coords <- one.play %>%
  filter(off_or_def == "offense") %>%
  select(frame_id, x, y) %>%
  slice(ol_chull_order)

ol_chull_poly <- sp::Polygon(ol_chull_coords, hole = F)
ol_chull_area <- ol_chull_poly@area

########
## NOW LET'S PLOT IT
########


one.play %>%
  the_dots(
    animated = TRUE,
    orientation = FALSE,
    convex = TRUE,
    segment_length = 6,
    segment_size = 3,
    dot_size = 4,
    animated_h = 4,
    animated_w = 8,
    animated_res = 150
  )



