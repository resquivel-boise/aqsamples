content_or_stop <- function(response, ...) {
  res <- httr::stop_for_status(response)
  if(inherits(res, "response")) {
    httr::content(res, ...)
  } else {
    res
  }
}

json_or_stop <- function(response) {
  jsonlite::fromJSON(content_or_stop(response, as = "text", encoding = "UTF-8"))
}

connect_to_samples <- function(url, api_token) {
  rapiclient::get_operations(
    rapiclient::get_api(paste0(url, "/api/swagger.json")),
    .headers = c("Authorization" = paste0("token ", api_token)),
    handle_response = json_or_stop
  )
}

paginated_get <- function(operation, ...) {
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
