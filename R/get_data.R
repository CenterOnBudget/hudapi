

# Main functions to load data from HUD API


#' Load geographic data from HUD API
#'
#' \code{get_geo()} loads geographic data for states, counties, or metro areas
#' from the HUD API. This function is particularly useful for looking up county
#' FIPS codes and METRO codes.
#'
#' @param geography Geography of data to pull. One of "state", "county", or
#'   "metro".
#' @param state For county-level data, what state to pull data for. Specify as
#'   an upper or lowercase two-letter state abbreviation. The 50 states plus DC,
#'   AS, GU, MP, PR, and VI are supported.
#' @param token HUD API
#'   \href{https://www.huduser.gov/portal/dataset/fmr-api.html}{access token}.
#'   Store your token in env var \code{HUD_API_TOKEN} to pass automatically
#'   (see \href{https://github.com/CenterOnBudget/hudapi}{README} for
#'   instructions).
#' @param drop_empty_cols If \code{TRUE} (default), drop empty columns in
#'   returned data.
#' @param tibble If \code{TRUE} (default), return data as a
#'   \href{https://tibble.tidyverse.org/index.html}{tibble}. Otherwise, return
#'   data as a base data frame.
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_geo <- function(geography, state = NULL,
                    token = NULL, drop_empty_cols = TRUE, tibble = TRUE) {

  # Check args -----------------------------------------------------------------

  token <- check_token(token)

  if (length(geography) > 1 || !(geography %in% c("state", "county", "metro"))) {
    stop("`geography` must be one of `state`, `county`, or `metro`", call. = FALSE)
  }

  if (geography == "county") {
    if (is.null(state)) {
      stop("If `geography` is `county`, you must specify a `state`", call. = FALSE)
    } else {
      check_state(state)
    }
  } else {
    if (!is.null(state)) {
      warning(
        "`state` ignored, you can only specify a `state` when `geography` is `county`",
        call. = FALSE
      )
    }
  }

  # Get data -------------------------------------------------------------------

  if (geography == "state") {
    endpoint <- "listStates"
  } else if (geography == "county") {
    endpoint <- glue::glue("listCounties/{state}")
  } else {
    endpoint <- "listMetroAreas"
  }

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = glue::glue("hudapi/public/fmr/{endpoint}"),
    httr::add_headers(Authorization = glue::glue("Bearer {token}"))
  )

  check_resp_code(resp$status_code)

  # Clean data -----------------------------------------------------------------

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8")
  )

  if (geography == "state") {
    # Add leading 0 to state FIPS codes
    parsed$state_num <- sprintf("%02d", as.integer(parsed$state_num))
  }

  if (geography == "county") {
    # Remove trailing 9s from county FIPS codes
    parsed$fips_code <- substr(parsed$fips_code, start = 1, stop = 5)
  }

  if (drop_empty_cols) {
    parsed <- drop_empty_cols(parsed)
  }

  # Return data ----------------------------------------------------------------

  if (tibble) {
    tibble::as_tibble(parsed)
  } else {
    parsed
  }
}


#' Load Fair Market Rents from HUD API
#'
#' \code{get_fmr()} loads Fair Market Rents for counties or metro areas within
#' a given state from the HUD API.
#'
#' @param geography Geography of data to pull. One of "county" or "metro".
#' @param state State to pull data for. Specify as an upper or lowercase
#'   two-letter state abbreviation. The 50 states plus DC, AS, GU, MP, PR, and
#'   VI are supported.
#' @param year Year to pull data for. Currently, years 2017 to 2020 are
#'   supported.
#' @inheritParams get_geo
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_fmr <- function(geography, state, year,
                    token = NULL, drop_empty_cols = TRUE, tibble = TRUE) {

  # Check args -----------------------------------------------------------------

  token <- check_token(token)

  if (length(geography) > 1 || !(geography %in% c("county", "metro"))) {
    stop("`geography` must be one of `county` or `metro`", call. = FALSE)
  }

  check_state(state)
  check_year(year)

  # Get data -------------------------------------------------------------------

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = glue::glue("hudapi/public/fmr/statedata/{state}"),
    query = list(year = year),
    httr::add_headers(Authorization = glue::glue("Bearer {token}"))
  )

  check_resp_code(resp$status_code)

  # Clean data -----------------------------------------------------------------

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8")
  )

  if (geography == "county") {
    output <- parsed$data[["counties"]]
    # Remove trailing 9s from county FIPS codes
    output$fips_code <- substr(output$fips_code, start = 1, stop = 5)
  } else {
    output <- parsed$data[["metroareas"]]
  }

  if (length(output) == 0) {
    return(output)
  }

  if (drop_empty_cols) {
    output <- drop_empty_cols(output)
  }

  # Return data ----------------------------------------------------------------

  if (tibble) {
    tibble::as_tibble(output)
  } else {
    output
  }
}


#' Load Income Limits from HUD API
#'
#' \code{get_il()} loads Income Limits for a given state, county, or
#' metro area from the HUD API.
#'
#' @param geography Geography of data to pull. One of "state", "county", or
#'   "metro".
#' @param entityid ID of entity to pull data for. If geography is "state", ID
#'   should be an upper or lowercase two-letter state abbreviation. The 50
#'   states are supported. If geography is "county", ID should be a 5-character
#'   county FIPS code. If geography is "metro", ID should be a 16-character
#'   METRO code. Using \code{get_geo()} to look up METRO codes is recommended.
#'
#'   NB: To pull Income Limits for DC, AS, GU, MP, PR, or VI, you must request
#'   data at the county or metro level of geography, as applicable.
#' @inheritParams get_fmr
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_il <- function(geography, entityid, year,
                   token = NULL, drop_empty_cols = TRUE, tibble = TRUE) {

  # Check args -----------------------------------------------------------------

  token <- check_token(token)

  if (length(geography) > 1 || !(geography %in% c("state", "county", "metro"))) {
    stop("`geography` must be one of `state`, `county`, or `metro`", call. = FALSE)
  }

  if (length(entityid) > 1 || !is.character(entityid)) {
    stop("Pass one `entityid` at a time as a string", call. = FALSE)
  }

  if (geography == "state") {
    check_state(entityid, plus_dc = FALSE, plus_other = FALSE)
  } else if (geography == "county") {
    if (nchar(entityid) != 5) {
      stop(
        "If `geography` is `county`, `entityid` should be 5-character county FIPS code",
        call. = FALSE
      )
    }
  } else {
    if (nchar(entityid) != 16) {
      stop(
        "If `geography` is `metro`, `entityid` should be 16-character METRO code",
        call. = FALSE
      )
    }
  }

  check_year(year)

  # Get data -------------------------------------------------------------------

  if (geography == "state") {
    path <- glue::glue("hudapi/public/il/statedata/{entityid}")
  } else if (geography == "county") {
    path <- glue::glue("hudapi/public/il/data/{entityid}99999") # Must add trailing 9s
  } else {
    path <- glue::glue("hudapi/public/il/data/{entityid}")
  }

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = path,
    query = list(year = year),
    httr::add_headers(Authorization = glue::glue("Bearer {token}"))
  )

  check_resp_code(resp$status_code)

  # Clean data -----------------------------------------------------------------

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    # Keep as default list
    simplifyVector = FALSE
  )

  data <- parsed$data
  income_limits <- c("low", "very_low", "extremely_low")
  data[income_limits] <- lapply(data[income_limits], unlist)
  data$size <- 1:8
  output <- as.data.frame(data, row.names = data$size)

  if (drop_empty_cols) {
    output <- drop_empty_cols(output)
  }

  # Return data ----------------------------------------------------------------

  if (tibble) {
    tibble::as_tibble(output)
  } else {
    output
  }
}

