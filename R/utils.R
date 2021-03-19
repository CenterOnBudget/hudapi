

# Small utility functions used to streamline main functions (in `get_data.R`)


# Check access token

check_token <- function(token) {

  # If non-`NULL` value was passed, return as given

  if (!is.null(token)) {
    message("Store your `token` in env var `HUD_API_TOKEN` to pass automatically")
    return(token)
  }

  # Otherwise, look for env var `HUD_API_TOKEN`

  if (Sys.getenv("HUD_API_TOKEN") != "") {
    Sys.getenv("HUD_API_TOKEN")
  } else {
    stop("You must provide a HUD API access `token`", call. = FALSE)
  }
}


# Check HUD API response

check_resp <- function(resp) {

  # Response code descriptions from HUD API documentation:
  # https://www.huduser.gov/portal/dataset/fmr-api.html

  # Lightly modified to ensure helpfulness
  resp_code_desc <- c(
    `200` = "Request was successful",
    `400` = "An invalid value was specified for one of the query parameters",
    `401` = "Authentication failure, check that you entered your access token correctly",
    `403` = "Not allowed to access this dataset API because you have not registered for it",
    `404` = "No data found using value you entered",
    `405` = "Unsupported method, only GET is supported",
    `406` = "Unsupported Accept Header value, must be application/json",
    `500` = "Internal server error occurred"
  )

  if (httr::http_type(resp) != "application/json") {
    stop("HUD API did not return JSON", call. = FALSE)
  }

  if (resp$status_code != 200) {
    stop(
      "HUD API request failed [", resp$status_code, "]: ",
      resp_code_desc[as.character(resp$status_code)],
      call. = FALSE
    )
  }
}


# Check if specified state is valid

check_state <- function(state, plus_dc = TRUE, plus_other = TRUE) {

  states <- datasets::state.abb

  if (plus_dc) {
    states <- c(states, "DC")
  }

  if (plus_other) {
    # American Samoa, Guam, Northern Mariana Islands, Puerto Rico, and
    # Virgin Islands
    states <- c(states, "AS", "GU", "MP", "PR", "VI")
  }

  if (length(state) != 1 || !is.character(state) || nchar(state) != 2) {
    stop(
      "Pass one `state` at a time using an upper ",
      "or lowercase two-letter state abbreviation",
      call. = FALSE
    )
  }

  if (!(toupper(state) %in% states)) {
    stop("Invalid `state`, see help page for supported states", call. = FALSE)
  }
}


# Check if specified year is valid

check_year <- function(year) {

  if (length(year) != 1 || !is.numeric(year)) {
    stop("Pass one `year` at a time as a number", call. = FALSE)
  }

  # Year availability is not well-documented
  if (!(year %in% 2017:2020)) {
    stop(
      "Invalid `year`, years 2017 to 2020 are supported",
      call. = FALSE
    )
  }
}


# Drop empty columns

drop_empty_cols <- function(df) {

  empty_cols <- vector(mode = "logical", length = ncol(df))
  names(empty_cols) <- names(df)

  for (i in seq_along(df)) {
    empty_cols[i] <- all(is.na(df[[i]])) || all(df[[i]] == "")
  }

  if (any(empty_cols)) {
    df[!empty_cols]
  } else {
    df
  }
}

