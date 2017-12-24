library(tidyverse)

api_key <- "d0021782e7d4973d951b98a8a6c86ce0"

### ----

lego_data

loop_data <- lego_data[[2]]

parts <- test$part
colors <- test$color

purrr::map_df(lego_data[2], function(loop_data){

  df <- loop_data
  df$part <- df$color <- NA

  df <- tibble::as_tibble(df)

  color <- color_list_to_df(list(loop_data$color))
  names(color) <- stringr::str_c("color_", names(color))

  part <- parts_list_to_df(list(loop_data$part))
  names(part) <- stringr::str_c("part_", names(part))
  names(part) <- stringr::str_replace_all(names(part), "part_part_", "part_")

  df
})

color_list_to_df(list(colors))
parts_list_to_df(list(parts))
