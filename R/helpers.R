response_codes <- function(status_code){

  status_df <- tibble::tribble(
      ~status_code, ~message,
      200,	"Success",
      201,	"Successfully created item",
      204,	"Item deleted successfully",
      400,	"Something was wrong with the format of your request",
      401,	"Unauthorized - your API key is invalid",
      403,	"Forbidden - you do not have access to operate on the requested item(s)",
      404,	"Item not found",
      429,	"Request was throttled - you are sending too many requests too fast."
    )

  out <- status_df[status_df$status_code == status_code, "message"]

  out <- unlist(out, use.names = FALSE)

  out
}

lego_get <- function(url, ..., api_key){

  auth <- paste("key", api_key)

  query = list(...)

  # Call the api
  api_call <- httr::GET(url, query = query,
                        httr::add_headers(Authorization = auth))

  if(httr::status_code(api_call) > 204){
    stop(response_codes(httr::status_code(api_call)))
  } else {
    message(response_codes(httr::status_code(api_call)))
  }

  # Collect data
  out <- list()

  api_data <- httr::content(api_call)

  if(is.null(api_data$results)){
    api_data <- null_to_na(api_data)
    return(api_data)
  }

  if(length(api_data$results) == 0){
    api_data$results <- NA
    api_data <- null_to_na(api_data)
    return(api_data)
  }

  out <- c(out, list(api_data$results))

  # While loop to deal with pagination
  while(!is.null(api_data$`next`)){
    message(paste("Pagenating to:", api_data$`next`))
    api_call <- httr::GET(api_data$`next`, httr::add_headers(Authorization = auth))
    api_data <- httr::content(api_call)
    out <- c(out, list(api_data$results))
  }

  # Flatten the list
  out <- purrr::flatten(out)

  # Set nulls to NA
  out <- null_to_na(out)

  # Return data
  out

}

null_to_na <- function(mylist){
  purrr::map(mylist, function(x){
    if(is.list(x)){
      null_to_na(x)
    } else {
      if(is.null(x)) NA else x
    }
  })
}

color_list_to_df <- function(lego_data){
  out <- purrr::map_df(lego_data, function(color){

    external_ids <- names(color$external_ids)

    col_df <- purrr::map_df(external_ids, function(external_id){
      ext_ids <- unlist(color$external_ids[[external_id]]$ext_ids)

      df <- tibble::tibble(
        external_id = external_id,
        ext_ids = ext_ids
      )

      ext_descrs <- color$external_ids[[external_id]]$ext_descrs
      ext_descrs <- purrr::map(ext_descrs, unlist)

      df$ext_descrs <- ext_descrs

      df <- tidyr::unnest(df, ext_descrs)

      df
    })

    external <- tidyr::nest(col_df, .key = external_ids)

    tibble::tibble(
      id = color$id,
      name = color$name,
      rgb = color$rgb,
      is_trans = color$is_trans,
      external_ids = external$external_ids
    )
  })

  out
}

parts_list_to_df <- function(lego_data){
  out <- purrr::map_df(lego_data, function(parts_data){

    if(length(parts_data$external_ids) != 0){
      part_df <- tibble::tibble(
        external_ids = names(parts_data$external_ids)
      )

      part_df$ids <- purrr::map(part_df$external_ids, function(ext_name){
        unlist(parts_data$external_ids[[ext_name]])
      })

      part_df <- tidyr::unnest(part_df, ids)

      external <- tidyr::nest(part_df, .key = external_ids)
    } else {
      external <- list()
      external$external_ids <- NA
    }

    tibble::tibble(
      part_num = parts_data$part_num,
      name = parts_data$name,
      part_cat_id = parts_data$part_cat_id,
      part_url = parts_data$part_url,
      part_img_url = parts_data$part_img_url,
      external_ids = external$external_ids
    )
  })

  out
}


