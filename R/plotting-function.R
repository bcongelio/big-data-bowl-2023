the_dots <- function(
    tracking_data,
    orientation = TRUE,
    convex = FALSE,
    dot_size = 6,
    segment_length = 2.5,
    segment_size = 1.5,
    numbers = TRUE,
    animated = TRUE,
    animated_h = 4,
    animated_w = 8,
    animated_res = 200,
    frame = NULL) {
  
  caption <- glue::glue("{tracking_data$game_id[1]} {tracking_data$down[1]}&{tracking_data$yards_to_go[1]}")
  
fig <- gg_field_team("PIT") +
  geom_point(data = tracking_data, aes(x, y),
              shape = ifelse(tracking_data$team == "football" | tracking_data$off_or_def == "offense", 9, 1),
              size = dot_size) +
  labs(caption = caption) +
  theme(plot.title = element_blank(),
        plot.margin = margin(.1, 0, .5, 0, "cm"),
        plot.caption = element_text(size = 8))

if (orientation == TRUE & "o" %in% names(tracking_data)) {
  
  fig <- fig +
    geom_segment(
      data = tracking_data,
      aes(x, y, xend = x + segment_length * o_x, yend = y + segment_length * o_y),
      size = segment_size
    )
}

if (convex == TRUE) {
  
  fig <- fig +
    geom_polygon(data = ol_chull_coords, aes(x = x, y = y), fill = "red", alpha = 0.2) +
    labs(color = '')
}

if (animated) {
  
  fig <- fig +
    gganimate::transition_time(tracking_data$frame_id)
  
  fig <- gganimate::animate(
    fig,
    height = animated_h, width = animated_w, units = "in",
    res = animated_res,
    nframes = n_distinct(tracking_data$frame_id),
    start_pause = 6,
    end_pause = 4)
  
}

return(fig)

}