

#' Load geographic data from the HUD API
#'
#' \code{get_geo()} loads geographic data for states, counties, or metro areas
#' from the HUD API. This function is particularly useful for looking up county
#' and METRO codes.
#'
#' @param geography Geography of data to pull. One of \code{"state"},
#'   \code{"county"}, or \code{"metro"}.
#' @param state If \code{geography} is \code{"county"}, what state to pull data
#'   for. Specify as a two-letter state abbreviation. The 50 states plus DC, AS,
#'   GU, MP, PR, and VI are supported.
#' @param token HUD API
#'   \href{https://www.huduser.gov/portal/dataset/fmr-api.html}{access token}.
#'   Defaults to environment variable \code{HUD_API_TOKEN}.
#' @param drop_empty_cols If \code{TRUE} (default), drop empty columns in
#'   returned data.
#' @param tibble If \code{TRUE} (default), return data as a
#'   \href{https://tibble.tidyverse.org/index.html}{tibble}. Otherwise, return
#'   data as a base data frame.
#' @param show_url If \code{TRUE}, show URL the request was sent to. Defaults to
#'   \code{FALSE}.
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_geo <- function(geography, state = NULL,
                    token = get_token(), drop_empty_cols = TRUE,
                    tibble = TRUE, show_url = FALSE) {

  # Check args -----------------------------------------------------------------

  check_token(token)

  if (!is_string(geography) || geography %!in% c("state", "county", "metro")) {
    stop('`geography` must be one of "state", "county", or "metro"', call. = FALSE)
  }

  if (geography == "county") {
    if (is.null(state)) {
      stop('If `geography` is "county", you must specify a `state`', call. = FALSE)
    }
    state <- check_state(state)
  } else {
    if (!is.null(state)) {
      warning(
        '`state` ignored, you can only specify a `state` when `geography` is "county"',
        call. = FALSE
      )
    }
  }

  # Get data -------------------------------------------------------------------

  if (geography == "state") {
    endpoint <- "listStates"
  } else if (geography == "county") {
    endpoint <- paste0("listCounties/", state)
  } else {
    endpoint <- "listMetroAreas"
  }

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = paste0("hudapi/public/fmr/", endpoint),
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  check_resp(resp, show_url = show_url)

  # Clean data -----------------------------------------------------------------

  output <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8")
  )

  stopifnot(is.data.frame(output))

  if (geography == "state") {
    # Add leading 0 to state FIPS codes
    output$state_num <- sprintf("%02d", as.integer(output$state_num))
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


#' Load Fair Market Rents from the HUD API
#'
#' \code{get_fmr()} loads Fair Market Rents for counties or metro areas within
#' a given state from the HUD API.
#'
#' @param geography Geography of data to pull. One of \code{"county"} or
#'   \code{"metro"}.
#' @param state State to pull data for. Specify as a two-letter state
#'   abbreviation. The 50 states plus DC, AS, GU, MP, PR, and VI are supported.
#' @param year Year to pull data for. Currently, years 2017 to 2020 are
#'   supported.
#' @inheritParams get_geo
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_fmr <- function(geography, state, year,
                    token = get_token(), drop_empty_cols = TRUE,
                    tibble = TRUE, show_url = FALSE) {

  # Check args -----------------------------------------------------------------

  check_token(token)

  if (!is_string(geography) || geography %!in% c("county", "metro")) {
    stop('`geography` must be one of "county" or "metro"', call. = FALSE)
  }

  state <- check_state(state)
  check_year(year, dataset = "fmr")

  # Get data -------------------------------------------------------------------

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = paste0("hudapi/public/fmr/statedata/", state),
    query = list(year = year),
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  check_resp(resp, show_url = show_url)

  # Clean data -----------------------------------------------------------------

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8")
  )

  data <- parsed$data

  if (!all(c("counties", "metroareas", "year") %in% names(data))) {
    stop("Fair Market Rents not parsed as expected", call. = FALSE)
  }

  if (geography == "county") {
    output <- data$counties
  } else {
    output <- data$metroareas
  }

  if (length(output) == 0) {
    # This seems to only occur when requesting metro data for states with no
    # metro areas
    message("HUD API did not return any data for your request")
    return(output)
  }

  stopifnot(is.data.frame(output))

  # Some columns in FMR data have non-syntactic names
  names(output) <- tolower(
    gsub(pattern = "[ -]+", replacement = "_", x = names(output))
  )

  output$year <- data$year

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


#' Load Income Limits from the HUD API
#'
#' \code{get_il()} loads Income Limits for a given state, county, or
#' metro area from the HUD API.
#'
#' @param geography Geography of data to pull. One of \code{"state"},
#'   \code{"county"}, or \code{"metro"}.
#' @param entityid ID of entity to pull data for. If \code{geography} is
#'   \code{"state"}, ID should be a two-letter state abbreviation. The 50 states
#'   are supported. If \code{geography} is \code{"county"}, ID should be a
#'   10-character county code. If \code{geography} is \code{"metro"}, ID should
#'   be a 16-character METRO code. Using \code{get_geo()} to look up county and
#'   METRO codes is recommended.
#' @inheritParams get_fmr
#' @return A tibble or base data frame with requested data.
#'
#' @export
get_il <- function(geography, entityid, year,
                   token = get_token(), drop_empty_cols = TRUE,
                   tibble = TRUE, show_url = FALSE) {

  # Check args -----------------------------------------------------------------

  check_token(token)

  if (!is_string(geography) || geography %!in% c("state", "county", "metro")) {
    stop('`geography` must be one of "state", "county", or "metro"', call. = FALSE)
  }

  if (!is_string(entityid)) {
    stop("Pass one `entityid` at a time as a string", call. = FALSE)
  }

  if (geography == "state") {
    entityid <- check_state(entityid, plus_dc = FALSE, plus_other = FALSE)
  } else if (geography == "county") {
    if (nchar(entityid) != 10) {
      stop(
        'If `geography` is "county", `entityid` must be a 10-character county code',
        call. = FALSE
      )
    }
  } else {
    if (nchar(entityid) != 16) {
      stop(
        'If `geography` is "metro", `entityid` must be a 16-character METRO code',
        call. = FALSE
      )
    }
  }

  check_year(year, dataset = "il")

  # Get data -------------------------------------------------------------------

  if (geography == "state") {
    path <- paste0("hudapi/public/il/statedata/", entityid)
  } else {
    path <- paste0("hudapi/public/il/data/", entityid)
  }

  resp <- httr::GET(
    url = "https://www.huduser.gov",
    path = path,
    query = list(year = year),
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  check_resp(resp, show_url = show_url)

  # Clean data -----------------------------------------------------------------

  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    # Keep as default list
    simplifyVector = FALSE
  )

  data <- parsed$data
  il <- c("low", "very_low", "extremely_low")

  if (!all(il %in% names(data)) || !all(lengths(data[il]) == 8)) {
    stop("Income Limits not parsed as expected", call. = FALSE)
  }

  data[il] <- lapply(data[il], unlist)
  data$size <- 1:8
  output <- as.data.frame(data, row.names = data$size)

  # Ensure that all names are lowercase for consistency
  names(output) <- tolower(names(output))

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
