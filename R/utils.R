

# Check access token

check_token <- function(token) {
  if (is.null(token)) {
    if (Sys.getenv("HUD_API_TOKEN") == "") {
      stop("You must provide a HUD API access `token`", call. = FALSE)
    }
    Sys.getenv("HUD_API_TOKEN")
  } else {
    message("Store your `token` in env var `HUD_API_TOKEN` to pass automatically")
    token
  }
}


# Check HUD API response

check_resp <- function(resp, show_url) {

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

  if (show_url) {
    message("URL: ", resp$url)
  }

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

  # Start with 50 states and build from there

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
      "Pass one `state` at a time using a two-letter state abbreviation",
      call. = FALSE
    )
  }

  state <- toupper(state)

  if (!(state %in% states)) {
    stop("Invalid `state`, see help page for supported states", call. = FALSE)
  }

  state # Return uppercase
}


# Check if specified year is valid

check_year <- function(year, dataset) {

  # Year availability is not well documented. See links below and
  # test manually.

  # FMR: https://www.huduser.gov/portal/datasets/fmr.html
  # IL: https://www.huduser.gov/portal/datasets/il.html

  lookup_years <- list(
    fmr = 2017:2021,
    il = 2017:2020
  )

  years <- lookup_years[[dataset]]

  if (length(year) != 1 || !is.numeric(year)) {
    stop("Pass one `year` at a time as a number", call. = FALSE)
  }

  if (!(year %in% years)) {
    stop(
      glue::glue(
        "Invalid `year`, years {years[1]} to {years[length(years)]} are supported"
      ),
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
