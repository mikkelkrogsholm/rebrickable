
lego_get_colors <- function(api_key){

  lego_data <- lego_get(url = "https://rebrickable.com/api/v3/lego/colors/",
           api_key = api_key)

  lego_data
}

lego_get_specific_color <- function(id, api_key){

  url <- paste0("https://rebrickable.com/api/v3/lego/colors/", id)

  lego_data <- lego_get(url = url, api_key = api_key)

  lego_data

}

lego_get_sets <- function(api_key){

  url <- "https://rebrickable.com/api/v3/lego/sets/"

  first_data <- lego_get(url, page = 1, api_key = api_key)

  pages <- ceiling(first_data$count / length(first_data$results))

  message(paste0("Getting data from ", pages, " pages"))

  lego_data <- purrr::map(1:pages, function(i){
    loop_data <- lego_get(url, page = 1, api_key = api_key)
    loop_data <- loop_data$results
    loop_data
  })

  lego_data

}
