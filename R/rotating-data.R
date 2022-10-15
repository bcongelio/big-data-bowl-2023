rotate_the_dots <- function(df) {
  
  if(!"play_direction" %in% names(df)) {
    message("Cannot find play directions. Inferring from offense and defense locations at snap.")
    
    df <- df %>%
      filter(event == "ball_snap", team != "football") %>%
      group_by(game_id, play_id, defensive_team) %>%
      summarize(mean_x = mean(x, na.rm = T)) %>%
      pivot_wider(names_from = defensive_team, values_from = mean_x, names_prefix = "x_") %>%
      ungroup() %>%
      mutate(
        play_direction = 
          ifelse(
            x_1 > x_0,
            "right",
            "left") %>%
          select(game_id, play_id, play_direction) %>%
          inner_join(df, by = c("game_id", "play_id")))
        
  }
  
  df <- df %>%
    mutate(
      to_left = ifelse(play_direction == "left", 1, 0),
      x = ifelse(to_left == 1, 120 - x, x),
      y = ifelse(to_left == 1, 160 / 3 - y, y),
      los_x = 100 - absolute_yardline_number,
      dist_from_los = x - los_x)
  
  if ("o" %in% names(df)) {
    df <- df %>%
      mutate(
        o = ifelse(to_left == 1, o + 180, o),
        o = ifelse(o > 360, 0 - 360, o),
        o_rad = pi * (o / 180),
        o_x = ifelse(is.na(o), NA_real_, sin(o_rad)),
        o_y = ifelse(is.na(o), NA_real_, cos(o_rad)))
  }
  
  if ("dir" %in% names(df)) {
    df <- df %>%
      mutate(
        dir = ifelse(to_left == 1, dir + 180, dir),
        dir = ifelse(dir > 360, dir - 360, dir),
        dir_rad = pi * (dir / 180),
        dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
        dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
        s_x = dir_x * s,
        s_y = dir_y * s,
        a_x = dir_x * a,
        a_y = dir_y * a)
  }
  
  return(df)
}