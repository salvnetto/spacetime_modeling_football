library(sf)
library(dplyr)
library(tidyr)

compute_ssi <- function(pred_diff, x_min = 70, x_max = 88.5, y_min = 24, y_max = 44) {
  coords <- st_coordinates(pred_diff)
  
  pred_diff %>%
    as_tibble() %>% 
    mutate(
      x = coords[, 1],
      y = coords[, 2]
    ) %>%
    filter(x >= x_min, x <= x_max, y >= y_min, y <= y_max) %>%
    group_by(across(any_of("time_sec"))) %>%
    summarize(
      # Net SSI considers both attacking and defensive presence
      SSI_Net = sum(mean, na.rm = TRUE),
      .groups = "drop"
    )
}



compute_adv_xST <- function(pred_diff, goal_x = 105, goal_y = 34) {
  coords <- st_coordinates(pred_diff)
  
  # Goal posts coordinates (assuming standard 7.32m goal width)
  post_top <- goal_y + (7.32 / 2)
  post_bottom <- goal_y - (7.32 / 2)
  
  pred_diff %>%
    as_tibble() %>%
    mutate(
      x = coords[, 1],
      y = coords[, 2],
      
      # 1. Distance to the center of the goal
      dist_to_goal = sqrt((x - goal_x)^2 + (y - goal_y)^2),
      
      # 2. Visible angle of the goal 
      dist_top = sqrt((x - goal_x)^2 + (y - post_top)^2),
      dist_bottom = sqrt((x - goal_x)^2 + (y - post_bottom)^2),
      
      denominator = 2 * dist_top * dist_bottom,
      cos_angle = if_else(denominator == 0, 0, (dist_top^2 + dist_bottom^2 - 7.32^2) / denominator),
      cos_angle = pmax(-1, pmin(1, cos_angle)),
      angle_rad = acos(cos_angle),
      
      # 3. Base Threat: combination of proximity and shooting angle
      base_threat = angle_rad * exp(-dist_to_goal / 25),
      
      # 4. Spatial Control: we only care about regions where attack > defense
      # (mean represents the difference field)
      space_control = if_else(mean > 0, mean, 0),
      
      # Final point threat
      point_xST = space_control * base_threat
    ) %>%
    group_by(across(any_of("time_sec"))) %>%
    summarize(
      xST = sum(point_xST, na.rm = TRUE),
      .groups = "drop"
    )
}

