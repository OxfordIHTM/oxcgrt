################################################################################
#
#'
#' Get policy actions and stringency data from JSON
#'
#' @param json A JSON string, URL or file created using [get_json_time()] or
#'   [get_json_actions()].
#'
#' @return A tibble of time series stringency index data if `json` is a time
#'   **series endpoint** or a named list of two tibbles (the first tibble is
#'   named `policyActions` and the second tibble is named `stringencyData`) if
#'   `json` is a **policy actions endpoint**.
#'
#' @examples
#' ## Get time series JSON endpoint
#' x <- get_json_time(from = "2020-10-29", to = "2020-10-31")
#'
#' ## Get time series stringency index data
#' get_data(x)
#'
#' ## Get policy actions JSON endpoint
#' x <- get_json_actions(ccode = "AFG", from = NULL, to = "2020-07-16")
#'
#' ## Get data on policy actions and stringency index
#' get_data(x)
#'
#' @export
#'
#
################################################################################

get_data <- function(json) {
  ## Check if json is a vector
  if (length(json) > 1) {
    json <- json[1]
    warning(
      "json has more than one value. Using first value only.",
      call. = TRUE, immediate. = TRUE
    )
  }

  ## Check if with internet and if API is working ----
  if (!curl::has_internet()) {
    message("No internet connection detected. Data cannot be retrieved.")
    return(NULL)
  } else {
    if (httr::http_error(x = json)) {
      message("URL to JSON data source is erroring. Data cannot be retrieved")
      return(NULL)
    } else {
      ## Extract data from JSON
      for (i in seq_len(3)) {
        x <- try(jsonlite::fromJSON(txt = json, flatten = TRUE))
        if (!inherits(x, "try-error")) break
      }

      if (inherits(x, "try-error")) {
        message("JSON data source cannot be reached right now. Try again later.")
        return(NULL)
      } else {
        if (stringr::str_detect(string = json, pattern = "[A-Z]{3}")) {
          ## Convert data.frames in list to tibble
          x[["policyActions"]] <- tibble::tibble(x[["policyActions"]]) |>
            dplyr::mutate(
              policyvalue = as.integer(.data$policyvalue),
              notes = as.character(.data$notes)
            )
          x[["stringencyData"]] <- dplyr::bind_rows(x[["stringencyData"]])
        } else {
          ## Convert list to data.frame
          y <- unlist(x[["data"]], recursive = FALSE)
          z <- lapply(X = y, FUN = unlist)
          df <- dplyr::bind_rows(z)

          ## Convert column classes to appropriate classes
          x <- df |>
            dplyr::mutate(
              date_value = as.Date(.data$date_value, format = "%Y-%m-%d"),
              confirmed = as.integer(.data$confirmed),
              deaths = as.integer(.data$deaths),
              stringency_actual = as.numeric(.data$stringency_actual),
              stringency = as.numeric(.data$stringency),
              stringency_legacy = as.numeric(.data$stringency_legacy),
              stringency_legacy_disp = as.numeric(.data$stringency_legacy_disp)
            )
        }

        ## Return data
        x
      }
    }
  }
}


################################################################################
#
#'
#' Get time series stringency index data from JSON
#'
#' @param json A JSON string, URL or file created using [get_json_time()]
#'
#' @return A tibble of time series stringency index data
#'
#' @examples
#' x <- get_json_time(from = "2020-07-18", to = "2020-07-20")
#'
#' get_data_time(x)
#'
#' @export
#'
#
################################################################################

get_data_time <- function(json) {
  ## Check if with internet
  if (!curl::has_internet()) {
    message("No internet connection detected. Data cannot be retrieved.")
    return(NULL)
  } else {
    if (httr::http_error(x = json)) {
      message("JSON data source seems to be broken. Data cannot be retrieved")
      return(NULL)
    } else {
      ## Extract data from JSON
      for (i in seq_len(3)) {
        x <- try(jsonlite::fromJSON(txt = json, flatten = TRUE))
        if (!inherits(x, "try-error")) break
      }

      if (inherits(x, "try-error")) {
        message("JSON data source cannot be reached right now. Try again later.")
        return(NULL)
      } else {
        ## Convert list to data.frame
        y <- unlist(x[["data"]], recursive = FALSE)
        z <- lapply(X = y, FUN = unlist)
        df <- dplyr::bind_rows(z)

        ## Convert column classes to appropriate classes
        df <- df |>
          dplyr::mutate(
            date_value = as.Date(.data$date_value, format = "%Y-%m-%d"),
            confirmed = as.integer(.data$confirmed),
            deaths = as.integer(.data$deaths),
            stringency_actual = as.numeric(.data$stringency_actual),
            stringency = as.numeric(.data$stringency),
            stringency_legacy = as.numeric(.data$stringency_legacy),
            stringency_legacy_disp = as.numeric(.data$stringency_legacy_disp),
            country_name = countrycode::countrycode(
              sourcevar = .data$country_code,
              origin = "iso3c",
              destination = "country.name",
              custom_match = c("RKS" = "Kosovo")
            )
          ) |>
          dplyr::relocate(.data$country_name, .after = .data$country_code)

        ## Return data
        df
      }
    }
  }
}


################################################################################
#
#'
#' Get policy actions data from JSON
#'
#' @param json A JSON string, URL or file created using [get_json_actions()] or
#'   a vector of JSON strings or URLs.
#'
#' @return A tibble of policy actions with their respective policy values for
#'   specified country/countries and specified date/dates.
#'
#' @examples
#' ## Get relevant JSON for Afghanistan on 01 January 2022
#' x <- get_json_actions(ccode = "AFG", from = NULL, to = "2022-01-01")
#'
#' ## Get data on policy actions
#' get_data_action(x)
#'
#' ## Get relevant JSON for Afghanistan and Philippines for whole month of
#' ## October
#' x <- get_json_actions(ccode = c("AFG", "PH"),
#'                       from = "2020-10-29",
#'                       to = "2020-10-30")
#'
#' ## Get data on policy actions
#' get_data_actions(x)
#'
#' @rdname get_data_action
#' @export
#'
#
################################################################################

get_data_action <- function(json) {
  ## Check if json is a vector
  if(length(json) > 1) {
    json <- json[1]
    warning("json has more than one value. Using first value only.",
            call. = TRUE, immediate. = TRUE)
  }

  ## Check if with internet
  if (!curl::has_internet()) {
    message("No internet connection detected. Data cannot be retrieved.")
    return(NULL)
  } else {
    if (httr::http_error(x = json)) {
      message("JSON data source seems to be broken. Data cannot be retrieved")
      return(NULL)
    } else {
      ## Extract data from JSON
      for (i in seq_len(3)) {
        x <- try(jsonlite::fromJSON(txt = json, flatten = TRUE))
        if (!inherits(x, "try-error")) break
      }

      if (inherits(x, "try-error")) {
        message("JSON data source cannot be reached right now. Try again later.")
        return(NULL)
      } else {
        ## Extract data from JSON
        x <- x[["policyActions"]]

        ## Tidy up policyActions data.frame and convert to tibble
        x <- x |>
          dplyr::mutate(
            policyvalue = ifelse(.data$policyvalue == "NA", NA, .data$policyvalue),
            policyvalue = as.integer(.data$policyvalue),
            notes = as.character(.data$notes),
            date_value = stringr::str_extract(
              json, pattern = "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
            ),
            date_value = as.Date(.data$date_value, format = "%Y-%m-%d"),
            country_code = stringr::str_extract(json, pattern = "[A-Z]{3}"),
            country_name = countrycode::countrycode(
              sourcevar = .data$country_code,
              origin = "iso3c",
              destination = "country.name",
              custom_match = c("RKS" = "Kosovo")
            )
          ) |>
          dplyr::relocate(.data$country_name, .before = .data$policy_type_code) |>
          dplyr::relocate(.data$country_code, .before = .data$country_name) |>
          dplyr::relocate(.data$date_value, .before = .data$country_code) |>
          tibble::tibble()

        ## Return data
        x
      }
    }
  }
}


################################################################################
#
#'
#' @rdname get_data_action
#' @export
#'
#
################################################################################

get_data_actions <- function(json) {
  ## Extract data from JSON
  x <- lapply(X = json, FUN = get_data_action) |>
    dplyr::bind_rows()

  ## Return data
  x
}

