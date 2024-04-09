#'
#' Codebook for the Oxford COVID-19 Government Response Tracker
#'
#' @format A tibble with 47 rows and 6 columns:
#'
#' | **Variable** | **Description** |
#' | :--- | :--- |
#' | `ID` | Policy indicator identifier |
#' | `Name` | Name of policy indicator |
#' | `Description` | Description of policy indicator |
#' | `Measurement` | Measurement of policy indicator |
#' | `Coding` | Coding of measurement |
#' | `Policy Group` | Name of group policy indicator |
#'
#' @source https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md
#'
"codebook"


#'
#' Example indicator data for sub-index calculations
#'
#' @format A tibble with 16 rows and 6 columns
#'
#' | **Variable** | **Description** |
#' | :--- | :--- |
#' | `indicator` | Policy indicator code |
#' | `value` | Policy indicator value |
#' | `flag_value` | Policy indicator flag value |
#' | `max_value` | Maximum value for policy indicator |
#' | `flag` | Does the policy indicator have a flag value? 1 = Yes; 0 = No
#' | `score` | Policy indicator score from 0 - 100 |
#'
#' @source https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md
#'
"indicatorData"


#'
#' Table of OxCGRT dataset files available from OxCGRT GitHub data repositories
#'
#' @format A tibble with 90 rows and 6 columns
#'
#' | **Variable** | **Description** |
#' | :--- | :--- |
#' | `version` | OxCGRT dataset version. Either **v1**, **v2**, or **final**
#' | `filename` | Filename of dataset file
#' | `path` | Path of dataset file
#' | `url` | GitHub URL to dataset file
#' | `url_raw` | GitHub raw URL to dataset file
#' | `sha` | SHA-1 hash value for dataset file
#'
"oxcgrt_data_files"

