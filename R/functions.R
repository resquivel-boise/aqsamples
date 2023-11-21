#' Stops when an HTTP error is received
#'
#' Returns a successful HTTP response, but stops if an error is received.
#'
#' @param response An httr response object.
#' @param ... Optional parameters for the httr:content() function.
#'
#' @return The httr response object.
content_or_stop <- function(response, ...) {
  res <- httr::stop_for_status(response)
  if(inherits(res, "response")) {
    httr::content(res, ...)
  } else {
    res
  }
}


#' Returns a dataframe of the JSON response
#'
#' Returns a data frame of the JSON response object, but stops if an error is received.
#'
#' @param response An httr response object.
#'
#' @return A dataframe of the JSON response object.
json_or_stop <- function(response) {
  jsonlite::fromJSON(content_or_stop(response, as = "text", encoding = "UTF-8"))
}


#' Creates an authenticated API client to an AQUARIUS Samples account
#'
#' Builds an a dynamic, authenticated client for your AQUARIUS Samples instance.
#'
#' @param url Base URL for the AQUARIUS Samples account. "https://{yourinstance}.aqsamples.com".
#' @param api_token API token for the AQUARIUS Samples account. Browse to "https://{yourinstance}.aqsamples.com/api" to retrieve the account's api_token.
#'
#' @return An authenticated client for the AQUARIUS Samples account.
#' @export
#'
#' @examples
#' Connect to AQUARIUS Samples and retrieve names of the labs
connect_to_samples <- function(url, api_token) {
  rapiclient::get_operations(
    rapiclient::get_api(paste0(url, "/api/swagger.json")),
    .headers = c("Authorization" = paste0("token ", api_token)),
    handle_response = json_or_stop
  )
}

get_paginated_data <- function(operation, ...) {
  response <- operation(...)
  all_objects <- response$domainObjects
  while (nrow(all_objects) < response$totalCount) {
    message(nrow(all_objects), " of ", response$totalCount, " items received.")
    response = operation(cursor = response$cursor, ...)
    if (nrow(response$domainObjects) < 1) {
      message("No more items received")
      break
    }
    all_objects <- jsonlite::rbind_pages(list(all_objects, response$domainObjects))
  }
  all_objects
}
