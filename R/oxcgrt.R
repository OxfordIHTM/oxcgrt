#'
#' An Interface to the Oxford COVID-19 Government Response Tracker API
#'
#' The **Oxford COVID-19 Government Response Tracker (OxCGRT)** tracks and
#' compares worldwide government responses to the COVID-19 pandemic rigorously
#' and consistently. **OxCGRT** makes available systematic information in a
#' consistent way, aiding those who require information have access to it
#' efficiently for their purposes. This package facilitates access to the
#' **OxCGRT** data via its API for R users.
#'
#' @docType package
#' @keywords internal
#' @name oxcgrt
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate relocate matches pull filter last_col
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr crossing pivot_longer
#' @importFrom countrycode countrycode countryname
#' @importFrom stringr str_extract str_detect str_remove_all
#' @importFrom httr http_error
#' @importFrom curl has_internet
#' @importFrom rlang .data
#' @importFrom readr read_csv
#' @importFrom utils download.file
#'
"_PACKAGE"

