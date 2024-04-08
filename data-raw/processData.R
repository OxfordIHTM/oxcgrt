## Libraries
library(xml2)
library(dplyr)
library(rvest)
library(stringr)
library(tibble)
library(gh)

## Codebook

json <- gh::gh(
  endpoint = "GET /repos/{owner}/{repo}/contents/{path}",
  owner = "OxCGRT",
  repo = "covid-policy-tracker",
  path = "documentation/codebook.md"
)

readLines(con = file_path)

rvest::session("https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md") %>%
  rvest::html_elements(css = ".js-snippet-clipboard-copy-unpositioned") |>
  rvest::html_table()

codebook <- xml2::read_html("https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md") %>%
  rvest::html_nodes(css = ".container-lg table") %>%
  rvest::html_table() %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(`Policy Group` = c(rep("Containment and closure policies", 15),
                                   rep("Economic policies", 5),
                                   rep("Health system policies", 12),
                                   rep("Vaccination policies", 14),
                                   "Miscellaneous policies"),
                ID = stringr::str_split(string = Name, pattern = "_", simplify = TRUE)[ , 1],
                Coding = stringr::str_replace_all(string = Coding, pattern = " 0", replacement = "; 0" ),
                Coding = stringr::str_replace_all(string = Coding, pattern = " 1", replacement = "; 1" ),
                Coding = stringr::str_replace_all(string = Coding, pattern = " 2", replacement = "; 2" ),
                Coding = stringr::str_replace_all(string = Coding, pattern = " 3", replacement = "; 3" ),
                Coding = stringr::str_replace_all(string = Coding, pattern = " 4", replacement = "; 4" ),
                Coding = stringr::str_replace_all(string = Coding, pattern = " Blank", replacement = "; Blank")) %>%
  tibble::tibble()

usethis::use_data(codebook, overwrite = TRUE, compress = "xz")

## Example data
x <- xml2::read_html("https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md") %>%
  rvest::html_elements(css = ".markdown-body table") %>%
  rvest::html_table()

indicatorData <- x[[3]][1:16, ]
names(indicatorData) <- c("indicator", "value", "flag_value", "", "max_value", "flag", "", "score")

indicatorData <- indicatorData %>%
  dplyr::select(indicator:flag_value, max_value:flag, score) %>%
  dplyr::mutate(flag = stringr::str_extract_all(string = flag, pattern = "[0-9]"),
                value = as.integer(value),
                flag_value = as.integer(flag_value),
                flag = as.integer(flag)) %>%
  tibble::tibble()

usethis::use_data(indicatorData, overwrite = TRUE, compress = "xz")


## Table of links to download various data versions

### Get contents of legacy repository ----
legacy_files <- gh::gh(
  endpoint = "GET /repos/{owner}/{repo}/git/trees/{tree_sha}?{query}",
  owner = "OxCGRT",
  repo = "covid-policy-tracker-legacy",
  tree_sha = "main",
  query = "recursive=true"
) |>
  jsonlite::toJSON() |>
  jsonlite::fromJSON() |>
  (\(x) x$tree)() |>
  tidyr::unnest(cols = c(path, mode, type, sha, url, size)) |>
  dplyr::filter(
    type == "blob",
    stringr::str_detect(path, ".csv|.xlsx")
  ) |>
  dplyr::mutate(
    version = ifelse(
      stringr::str_detect(string = path, pattern = "legacy_data_20200425"),
      "v1", "v2"
    ),
    filename = basename(path),
    url_raw = file.path(
      "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker-legacy/main",
      path
    )
  ) |>
  dplyr::select(version, filename, path, url, url_raw, sha)

### Get contents of current repository ----
current_files <- gh::gh(
  endpoint = "GET /repos/{owner}/{repo}/git/trees/{tree_sha}?{query}",
  owner = "OxCGRT",
  repo = "covid-policy-dataset",
  tree_sha = "main",
  query = "recursive=true"
) |>
  jsonlite::toJSON() |>
  jsonlite::fromJSON() |>
  (\(x) x$tree)() |>
  tidyr::unnest(cols = c(path, mode, type, sha, url, size)) |>
  dplyr::filter(
    type == "blob",
    stringr::str_detect(path, ".csv|.xlsx")
  ) |>
  dplyr::mutate(
    version = "final",
    filename = basename(path),
    url_raw = file.path(
      "https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main",
      path
    )
  ) |>
  dplyr::select(version, filename, path, url, url_raw, sha)

oxcgrt_data_files <- rbind(legacy_files, current_files)

usethis::use_data(oxcgrt_data_files, overwrite = TRUE, compress = "xz")
