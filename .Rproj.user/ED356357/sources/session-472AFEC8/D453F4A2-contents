library(tidyverse)
library(ngscleanR)
library(piggyback)

#################################
### CLEANING FUNCTION FOR BIG DATA BOWL
### note: this builds off of Ben Baldwin's ngscleanR package, but with edits to make it work with 2022 data
#################################

### testing with just one week of BDB data
week1 <- readr::read_csv(
  file = "./big-data-bowl-data/week1.csv"
)

### note: testing here
week1 <- week1 %>%
  filter(gameId == "2021090900")

### using ngscleanR to rotate data
week1.cleaned <- clean_and_rotate(week1)

### now just selecting variables needed
week1.cleaned <- week1.cleaned %>%
  select(game_id, play_id, frame_id, time, jersey_number, team, play_direction,
         x, y, s, a, dis, o, dir, event, o_x, o_y, dir_x, s_x, s_y, a_x, a_y)

### now reading in plays as provided by big data bowl
plays <- readr::read_csv(
  file = "./big-data-bowl-data/plays.csv"
)

### combining week1.cleaned with plays
combined.data <- left_join(week1.cleaned, plays, by = c("game_id" = "gameId", "play_id" = "playId"))

### now bringing in nflreadR data to include air yards, epa, etc.
pbp <- nflreadr::load_pbp(2021) %>%
  select(old_game_id, play_id, posteam, defteam, yardline_100,score_differential, drive, goal_to_go, pass_location, air_yards, yards_after_catch,
         td_prob, epa, air_epa, yac_epa, comp_air_epa, comp_yac_epa, first_down_pass, third_down_converted, fourth_down_converted,
         qb_hit, sack, complete_pass, passer_player_name, receiver_player_name, tackle_for_loss_1_player_name, tackle_for_loss_2_player_name,
         qb_hit_1_player_name, qb_hit_2_player_name, solo_tackle_1_player_name, solo_tackle_2_player_id, sack_player_name,
         half_sack_1_player_name, half_sack_2_player_name, cp, qb_epa, xyac_epa, xyac_mean_yardage, xyac_median_yardage, 
         xyac_success, xyac_fd, pass_oe)

### `old_game_id` comes in as character - must switch to numeric
pbp$old_game_id <- as.numeric(as.character(pbp$old_game_id))

### merging pbp date in with combined.data now
combined.data <- left_join(combined.data, pbp, by = c("game_id" = "old_game_id", "play_id" = "play_id"))

### bringing in team colors to associate for plotting
colors <- nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color, team_color2)

### merging colors into data
combined.data <- left_join(combined.data, colors, by = c("posteam" = "team_abbr"))

### quickly renaming colors to bring in defensive team colors
colors <- colors %>%
  rename(
    def.team_color = team_color,
    def.team_color2 = team_color2)

### merging defensive colors into data
combined.data <- left_join(combined.data, colors, by = c("defteam" = "team_abbr"))

### reading in pff scouting data
pff.data <- readr::read_csv(
  file = "./big-data-bowl-data/pffScoutingData.csv"
)

### merging pff scouting data into combined data
combined.data <- left_join(combined.data, pff.data, by = c("game_id" = "gameId", "play_id" = "playId"))

### reading in nfl provided player data
player.data <- readr::read_csv(
  file = "./big-data-bowl-data/players.csv"
) %>%
  select(nflId, displayName)

### merging with combined data
combined.data <- left_join(combined.data, player.data, by= c("nflId" = "nflId"))

###################
## PREP OF THE DATA IS COMPLETE AT THIS POINT
###################

week1.cleaned %>%
  filter(play_id == 97) %>%
  plot_play(
    animated = TRUE,
    segment_length = 2,
    segment_size = 1,
    dot_size = 4
  )