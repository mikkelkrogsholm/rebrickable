
lego_get_colors <- function(api_key){

  url <-  "https://rebrickable.com/api/v3/lego/colors/"

  lego_data <- lego_get(url = url, api_key = api_key)

  message("Converting to tibble")
  out <- color_list_to_df(lego_data)

  out
}

lego_get_specific_color <- function(id, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/colors/", id)

  lego_data <- lego_get(url = url, api_key = api_key)

  lego_data <- list(lego_data)

  out <- color_list_to_df(lego_data)

  out

}

lego_get_categories <- function(api_key){

  url <- "https://rebrickable.com/api/v3/lego/part_categories/"

  lego_data <- lego_get(url = url, api_key = api_key)

  message("Converting to tibble")
  out <- purrr::map_df(lego_data, tibble::as_tibble)

  out

}

lego_get_specific_category <- function(id, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/part_categories/", id)

  lego_data <- lego_get(url = url, api_key = api_key)

  lego_data <- list(lego_data)

  message("Converting to tibble")
  out <- purrr::map_df(lego_data, tibble::as_tibble)

  out

}

lego_get_parts <- function(api_key){

  url <- "https://rebrickable.com/api/v3/lego/parts/"

  lego_data <- lego_get(url = url, api_key = api_key)

  message("Converting to tibble")

  out <- parts_list_to_df(lego_data)

  out

}

lego_get_part_num <- function(part_num, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/parts/", part_num)

  lego_data <- lego_get(url = url, api_key = api_key)

  out <- tibble::as_tibble(lego_data)

  out
}

lego_get_part_num_colors <- function(part_num, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/parts/", part_num, "/colors")

  lego_data <- lego_get(url = url, api_key = api_key)

  out <- purrr::flatten(lego_data)

  out <- tibble::as_tibble(out)

  out

}

lego_get_part_num_color_id <- function(part_num, color_id, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/parts/",
                part_num, "/colors/", color_id)

  lego_data <- lego_get(url = url, api_key = api_key)

  out <- purrr::flatten(lego_data)

  out <- tibble::as_tibble(out)

  out

}

lego_get_sets <- function(api_key){

  url <- "https://rebrickable.com/api/v3/lego/sets/"

  lego_data <- lego_get(url = url, api_key = api_key)

  message("Converting to tibble")
  out <- purrr::map_df(lego_data, tibble::as_tibble)

  out

}

lego_get_set_num <- function(set_num, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/sets/", set_num)

  lego_data <- lego_get(url = url, api_key = api_key)

  lego_data <- list(lego_data)

  message("Converting to tibble")
  out <- purrr::map_df(lego_data, tibble::as_tibble)

  out
}

lego_get_set_num_alternates <- function(set_num, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/sets/", set_num, "/alternates")

  lego_data <- lego_get(url = url, api_key = api_key)

  lego_data <- list(lego_data)

  message("Converting to tibble")
  out <- purrr::map_df(lego_data, tibble::as_tibble)

  out

}

lego_get_set_num_parts <- function(set_num, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/sets/", set_num, "/parts")

  lego_data <- lego_get(url = url, api_key = api_key)


  out

}
