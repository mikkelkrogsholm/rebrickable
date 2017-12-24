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

  api_call <- httr::GET(url, query = query,
                        httr::add_headers(Authorization = auth))

  if(httr::status_code(api_call) > 204){
    stop(response_codes(httr::status_code(api_call)))
  } else {
    message(response_codes(httr::status_code(api_call)))
  }

  api_data <- httr::content(api_call)

  api_data

}
