

# Small utility functions used to streamline main functions (in `get_data.R`)


# Check access token
#
# Check if user passed an access token. If non-`NULL` value was passed, return
# as given. Otherwise, check for a `HUD_API_TOKEN` environment variable and
# return it if it exists. If there is no `HUD_API_TOKEN` environment variable,
# throw error.
#
check_token <- function(token) {

  if (is.null(token)) {
    if (Sys.getenv("HUD_API_TOKEN") != "") {
      Sys.getenv("HUD_API_TOKEN")
    } else {
      stop("You must provide a HUD API access `token`", call. = FALSE)
    }
  } else {
    token
  }
}


# Give helpful error message for non-200 response code
#
# Check response code of HUD API call and, if non-200 response code, return
# an error with a response code description provided by HUD API documentation.
# If response code is 200 (OK), return nothing.
#
check_resp_code <- function(resp_code) {

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

  if (resp_code != 200) {
    stop(resp_code_desc[as.character(resp_code)], call. = FALSE)
  }
}


# Check if specified state is valid
#
# Check if specified state is a valid option for the HUD API. If more than one
# state is passed or an invalid state is passed, throw error. Otherwise, return
# nothing.
#
check_state <- function(state) {

  # Must use `::` to resolve R CMD check note
  states <- c(datasets::state.abb, "DC", tolower(datasets::state.abb), "dc")

  if (length(state) > 1 || !(state %in% states)) {
    stop(
      "Pass one `state` at a time using an upper ",
      "or lowercase two-letter state abbreviation",
      call. = FALSE
    )
  }
}


# Check if specified year is valid
#
# Check is specified year is a valid option for the HUD API. If more than one
# year is passed or an invalid year is passed, throw error. Otherwise, return
# nothing.
#
check_year <- function(year) {

  # Year availability is not well-documented
  if (length(year) > 1 || !(year %in% 2017:2020)) {
    stop(
      "Data only available one `year` at a time for years 2017 through 2020",
      call. = FALSE
    )
  }
}


# Drop empty columns
#
# Drop columns that are all `NA` or empty strings from a data frame, if any
# exist.
#
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

