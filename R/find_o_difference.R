find_o_diff <- function(df, prefix = "qb") {
  
  name_x <- sym(paste0(prefix, "_x"))
  name_y <- sym(paste0(prefix, "_y"))
  
  new_column <- paste0("o_to_", prefix)
  
  df <- df %>%
    mutate(
      dis_x = {{name_x}} - x,
      dis_y = {{name_y}} - y,
      
      tmp = atan2(dis_y, dis_x) * (180 / pi),
      tmp = (360 - tmp) + 90,
      tmp = case_when(tmp < 0 ~ tmp + 360,
                      tmp > 360 ~ tmp - 360,
                      TRUE ~ tmp),
      
      diff = abs(o - tmp),
      
      diff = abs(o - tmp),
      
      !!new_column := pmin(360 - diff, diff)) %>%
        select(-diff, -tmp)
      
    return(df)
}