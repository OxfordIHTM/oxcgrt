#'
#' Get legacy data version 1
#'
#' @param type A character value for type of dataset to retrieve. Either
#'   *"complete"* for the full dataset or *"timeseries"* for the disaggregated
#'   policy indicators datasets. Default is *"complete"*.
#' @param pcode A character value for policy indicator code to retrieve if
#'   `type` is *"timeseries"*. Ignored if `type` is *"complete"*. Can be one of
#'   *"s1"*, *"s2"*, *"s3"*, *"s4"*, *"s5"*, *"s6"*, *"s7"*, *"s8"*, *"s9"*,
#'   *"s10"*, *"s11"*, *"s12"*, or *"s13"*.
#' @param notes Logical. Should *"complete"* dataset be retrieved with notes or
#'   not. Default is TRUE. Ignored if `type` is *"timeseries"*.
#' @param directory If NULL (default), required dataset is read into current
#'   environment. If specified, required dataset is downloaded into this
#'   directory with the filename given by the OxCGRT.
#' @param tidy Logical. Should *"timeseries"* dataset be converted to tidy
#'   format? Default is FALSE.
#'
#' @returns If `directory` is NULL, a tibble (or a list of two tibbles) of the
#'   specified dataset. Otherwise, a file path/s to the downloaded file/s.
#'
#' @examples
#' get_data_v1_complete()
#' get_data_v1_timeseries()
#' get_data_v1()
#'
#' @rdname get_data_v1
#' @export
#'

get_data_v1 <- function(type = c("complete", "timeseries"),
                        pcode = c("s1", "s2", "s3", "s4", "s5",
                                  "s6", "s7", "s11", "s12", "s13"),
                        notes = TRUE,
                        directory = NULL,
                        tidy = FALSE) {
  ## Get type value ----
  type <- match.arg(type)

  ## Get pcode ----
  pcode <- match.arg(pcode)

  ## Retrieve specified data ----
  if (type == "complete") {
    x <- get_data_v1_complete(notes = notes, directory = directory)
  } else {
    x <- get_data_v1_timeseries(
      pcode = pcode, directory = directory, tidy = tidy
    )
  }

  ## Return x ----
  x
}

#'
#' @rdname get_data_v1
#' @export
#'

get_data_v1_complete <- function(notes = TRUE, directory = NULL) {
  ## Get data files ----
  data_files <- oxcgrt::oxcgrt_data_files

  ## Get download link ----
  if (notes) {
    link <- data_files |>
      dplyr::filter(
        .data$version == "v1",
        stringr::str_detect(string = .data$filename, pattern = "withnotes")
      ) |>
      dplyr::pull(.data$url_raw)
  } else {
    link <- data_files |>
      dplyr::filter(.data$filename == "OxCGRT_20200425 LEGACY.csv") |>
      dplyr::pull(.data$url_raw)
  }

  ## Test internet and link ----
  if (!curl::has_internet()) {
    warning(
      "No internet connection detected. Data cannot be retrieved. Returning NULL."
    )
    x <- NULL
  } else {
    if (httr::http_error(x = link)) {
      warning(
        "Download URL for data is currently unavailable. Returning NULL."
      )
      x <- NULL
    } else {
      ## Download or read dataset ----
      if (is.null(directory)) {
        x <- readr::read_csv(
          file = link,
          col_select = !dplyr::last_col(),
          name_repair = "unique_quiet", show_col_types = FALSE
        )

        x$Date <- strptime(x$Date, format = "%Y%m%d") |>
          as.Date()
      } else {
        x <- file.path(
          directory,
          oxcgrt::oxcgrt_data_files |>
            dplyr::filter(.data$url_raw == link) |>
            dplyr::pull(.data$filename)
        )

        download.file(url = link, destfile = x)
      }
    }
  }

  ## Return output ----
  x
}


#'
#' @rdname get_data_v1
#' @export
#'

get_data_v1_timeseries <- function(pcode = c("s1", "s2", "s3", "s4", "s5",
                                             "s6", "s7", "s11", "s12", "s13"),
                                   directory = NULL,
                                   tidy = FALSE) {
  ## Get pcode value ----
  pcode <- match.arg(pcode)

  ## Get data files ----
  data_files <- oxcgrt::oxcgrt_data_files

  ## Get download link ----
  link <- data_files |>
    dplyr::filter(
      stringr::str_detect(
        string = .data$filename, pattern = paste0(pcode, "_")
      )
    ) |>
    dplyr::pull(.data$url_raw)

  lapply(link, httr::http_error)

  if (!curl::has_internet()) {
    warning(
      "No internet connection detected. Data cannot be retrieved. Returning NULL."
    )
    x <- NULL
  } else {
    if (any(lapply(link, httr::http_error) |> unlist())) {
      warning(
        "Download URL for data is currently unavailable. Returning NULL."
      )
      x <- NULL
    } else {
      ## Download or read dataset? ----
      if (is.null(directory)) {
        x <- lapply(
          X = link, FUN = readr::read_csv, na = c("", "NA", "."),
          show_col_types = FALSE, name_repair = "unique_quiet"
        )

        names(x) <- link |> basename() |>
          stringr::str_remove_all(pattern = "%2020200425%20LEGACY.csv")

        ## Tidy up the dataset? ----
        if (tidy) {
          if (pcode %in% paste0("s", 1:6)) {
            x <- lapply(
              X = x,
              FUN = tidyr::pivot_longer,
              cols = dplyr::matches(match = "[0-9]{2}"),
              names_to = "Date"
            )

            names(x[[1]]) <- c("CountryName", "CountryCode", "Date", names(x)[1])
            names(x[[2]]) <- c("CountryName", "CountryCode", "Date", names(x)[2])

            x <- dplyr::full_join(
              x[[1]], x[[2]], by = c("CountryName", "CountryCode", "Date")
            ) |>
              dplyr::relocate(!dplyr::matches("general"), .after = "Date") |>
              dplyr::mutate(Date = as.Date(.data$Date, format = "%d%b%Y"))
          } else {
            x <- tidyr::pivot_longer(
              data = x[[1]],
              dplyr::matches(match = "[0-9]{2}"),
              names_to = "Date",
              values_to = names(x)
            ) |>
              dplyr::mutate(Date = as.Date(.data$Date, format = "%d%b%Y"))

            names(x)[1:2] <- c("CountryName", "CountryCode")
          }
        } else {
          names(x[[1]])[1:2] <- c("CountryName", "CountryCode")
          names(x[[2]])[1:2] <- c("CountryName", "CountryCode")
        }
      } else {
        x <- file.path(
          directory,
          oxcgrt::oxcgrt_data_files |>
            dplyr::filter(.data$url_raw %in% link) |>
            dplyr::pull(.data$filename)
        )

        Map(
          f = utils::download.file,
          url = as.list(link),
          destfile = as.list(x)
        )
      }
    }
  }

  ## Return x ----
  x
}

