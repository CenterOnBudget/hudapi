

is_number <- function(x) is.numeric(x) && length(x) == 1
is_string <- function(x) is.character(x) && length(x) == 1
`%!in%` <- function(x, table) match(x, table, nomatch = 0) == 0


get_token <- function() {
  token <- Sys.getenv("HUD_API_TOKEN")

  if (token == "") {
    stop(
      "HUD API access token not found, supply with `token` argument or env var `HUD_API_TOKEN`",
      call. = FALSE
    )
  }

  token
}


check_token <- function(token) {
  if (!is_string(token) || token == "") {
    stop("`token` must be a non-empty string", call. = FALSE)
  }
}


check_resp <- function(resp, show_url) {
  if (show_url) {
    message("URL: ", resp$url)
  }

  if (httr::http_type(resp) != "application/json") {
    stop("HUD API did not return JSON", call. = FALSE)
  }

  # Response code descriptions from HUD API documentation:
  # https://www.huduser.gov/portal/dataset/fmr-api.html

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

  if (resp$status_code != 200) {
    stop(
      "HUD API request failed [", resp$status_code, "]: ",
      resp_code_desc[as.character(resp$status_code)],
      call. = FALSE
    )
  }
}


check_state <- function(state, plus_dc = TRUE, plus_other = TRUE) {
  states <- datasets::state.abb

  if (plus_dc) {
    states <- c(states, "DC")
  }

  if (plus_other) {
    states <- c(states, "AS", "GU", "MP", "PR", "VI")
  }

  if (!is_string(state) || nchar(state) != 2) {
    stop(
      "Pass one `state` at a time using a two-letter state abbreviation",
      call. = FALSE
    )
  }

  state <- toupper(state)

  if (state %!in% states) {
    stop("Invalid `state`, see help page for supported states", call. = FALSE)
  }

  state
}


check_year <- function(year, dataset) {
  if (!is_number(year)) {
    stop("`year` must be a number", call. = FALSE)
  }

  # FMR: https://www.huduser.gov/portal/datasets/fmr.html
  # IL: https://www.huduser.gov/portal/datasets/il.html

  lookup <- list(
    fmr = 2017:2021,
    il = 2017:2020
  )

  years <- lookup[[dataset]]

  if (year %!in% years) {
    stop(
      "Invalid `year`, years ", min(years), " to ", max(years), " are currently supported",
      call. = FALSE
    )
  }
}


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
