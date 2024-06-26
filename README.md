
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oxcgrt: An Interface to the Oxford COVID-19 Government Response Tracker API <img src="man/figures/logo.png" width="200" align="right" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/oxcgrt)](https://CRAN.R-project.org/package=oxcgrt)
[![CRAN](https://img.shields.io/cran/l/oxcgrt.svg)](https://CRAN.R-project.org/package=oxcgrt)
[![CRAN](http://cranlogs.r-pkg.org/badges/oxcgrt)](https://CRAN.R-project.org/package=oxcgrt)
[![CRAN](http://cranlogs.r-pkg.org/badges/grand-total/oxcgrt)](https://CRAN.R-project.org/package=oxcgrt)
[![R-CMD-check](https://github.com/OxfordIHTM/oxcgrt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OxfordIHTM/oxcgrt/actions/workflows/R-CMD-check.yaml)
![test-coverage](https://github.com/OxfordIHTM/oxcgrt/workflows/test-coverage/badge.svg)
[![codecov](https://codecov.io/gh/OxfordIHTM/oxcgrt/graph/badge.svg?token=uUS5l7zPBs)](https://codecov.io/gh/OxfordIHTM/oxcgrt)
[![CodeFactor](https://www.codefactor.io/repository/github/oxfordihtm/oxcgrt/badge)](https://www.codefactor.io/repository/github/oxfordihtm/oxcgrt)
<!-- badges: end -->

The [Oxford COVID-19 Government Response Tracker
(OxCGRT)](https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker)
is a project that gathered data on government measures taken to address
COVID-19 from 2020 to 2023 rigorously and consistently. It involved over
1500 volunteers and provided real-time information on various policy
responses worldwide up to 2022. As of May 2023, most countries had
lifted their pandemic-related policies, prompting the project to shift
its focus to analysing the collected data and conducting research on the
impacts and determinants of these responses.

## The OxCGRT data

The OxCGRT has made its datasets available, including the different
versions, in a systematic and consistent way so as to aid those who
require information have access to it efficiently. There have been three
release versions of the OxCGRT datasets with the version dictated upon
by its data structure which in turn is based on the methodology that the
OxCGRT research team have developed and employed.

### Legacy version 1

**Start date:** March 2020

**Retirement date:** 25 April 2020

The legacy version 1 dataset was in place before 25 April 2020. The
datasets have the 13 *‘S’* indicators that the OxCGRT used in the months
of March and April of 2020, early on at the start of the pandemic.
**S1-S7** are policy decisions relating to various kinds of public
gatherings recorded on an ordinal scale. **S8-S11** are financial
indicators recorded as continuous variables. **S12** and **S13** relate
to COVID-19 testing and contact tracing. **S1-S6** are further
classified as either *“targeted”* (meaning they apply only in a
geographically concentrated area) or *“general”* (meaning they apply
throughout the entire jurisdiction). This data structure is further
described in this [working
paper](https://www.bsg.ox.ac.uk/sites/default/files/2020-04/BSG-WP-2020-031-v4.0_0.pdf).

The legacy version 1 dataset produced the first version (or legacy
version) of the *stringency index*. The methodology used to calculate
the *legacy stringency index* is found
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md#april-2020-legacy-stringency-index).

The legacy version 1 dataset is currently available from [OxCGRT’s
legacy GitHub
repository](https://github.com/OxCGRT/covid-policy-tracker-legacy) at
this
[directory](https://github.com/OxCGRT/covid-policy-tracker-legacy/blob/main/legacy_data_20200425).
The legacy version 1 dataset was also briefly available via and
application programming interface (API) but was immediately superseded
by a second version when legacy version 2 of the dataset was initiated
(see next section).

### Legacy version 2

**Start date:** 28 April 2020

**Retirement date:** 27 July 2022

OxCGRT expanded to 21 indicators of government response. Full
descriptions of the policy indicators and their meaning can be found
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md#indicators-for-targeted-policies).
This version also included statistics on the number of reported Covid-19
cases and deaths in each country. These were taken from the [JHU CSSE
data repository](https://github.com/CSSEGISandData/COVID-19) for all
countries and the US States. The OxCGRT legacy version 2 datasets also
included some subnational data for US states, Brazilian States, Indian
States, UK devolved nations, Canadian provinces and territories, and
Chinese provinces.

In addition to an updated *stringency index*, four additional indices
that aggregate the data into a single number were developed. These are
described
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md#indices)
and the calculation defined
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md#index-calculation).

The OxCGRT legacy version 2 datasets is currently available from
[OxCGRT’s legacy GitHub
repository](https://github.com/OxCGRT/covid-policy-tracker-legacy) at
this
[directory](https://github.com/OxCGRT/covid-policy-tracker-legacy/blob/main/legacy_data_202207).
This version of the OxCGRT datasets was also made available via a
[version 2 of the OxCGRT
API](https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker/covid-tracker-api)
which is formally declared as unsupported but continue to still be
accessible.

### Current and final dataset

**Start date:** July 2022

**Date of final release:** June 2023

From the end of July 2022, the data structure was again evolved to take
into account the need to differentiate policies applying to vaccinated
and unvaccinated populations. This brought about breaking changes to the
previous data structure and the need to produce additional data file
outputs. The OxCGRT stopped publishing real-time updates at the end of
2022. A final version of the OxCGRT dataset is available at
<https://github.com/OxCGRT/covid-policy-dataset>. The final version of
the dataset has more jurisdictions and more consistency in data format
between files. No API support is provided for this version of the
dataset.

## What does `oxcgrt` do?

The `oxcgrt` package facilitates access to the various OxCGRT data
versions for [R](https://cran.r-project.org) users. This package also
includes functions to calculate the various OxCGRT indices. This package
is aimed at [R](https://cran.r-project.org) users who use or plan to use
the OxCGRT data for their research or for other academic purposes or who
develop or want to develop other metrics or indices that build on the
OxCGRT approach.

The `oxcgrt` package has two main sets of functions that:

1.  Retrieve various versions of the OxCGRT datasets (`get_*`
    functions); and,

2.  Calculate various OxCGRT indicators, sub-indices and indices
    (`calculate_*` functions).

There are other R packages that provide access to data from the OxCGRT.
The [`COVID19` package](https://cran.r-project.org/package=COVID19) and
the [`oxcovid19` package](https://como-ph.github.io/oxcovid19/) are just
two examples of these. However, all these packages provide access to the
OxCGRT data as *data dumps* and only for the time-series of the
stringency index per country. To our knowledge, the `oxcgrt` package is
the only R package currently that provides an interface to the various
versions of the OxCGRT datasets available. Also, the `oxcgrt` package
provides functions to calculate the OxCGRT sub-indices and indices based
on their methodology. None of the other R packages that we have seen and
reviewed have this functionality.

## Installation

You can install the released version of oxcgrt from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("oxcgrt")
```

And the development version from [GitHub](https://github.com/) with:

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("OxfordIHTM/oxcgrt")
```

## Usage

### The `oxcgrt` data retrieval workflow via API

The *retrieve data* functions are based on the OxCGRT’s JSON API
described [here](https://covidtrackerapi.bsg.ox.ac.uk). Two API
endpoints are provided: 1) endpoint for JSON providing data for
stringency index by country over time; and, 2) endpoint for JSON
providing data on policy actions and stringency index for a specific
country on a specific day.

For each of these endpoints, the data retrieval workflow is composed of
two steps: first is the creation of the appropriate API URL query; and,
second is the retrieval of the appropriate data as per query into a
data.frame structure usable in R. This workflow is show in code below:

``` r
## Load oxcgrt package
library(oxcgrt)

## Step 1: Create the appropriate API URL query for time series data from 
## 1 December 2022 up to current day
query <- get_json_time(from = "2022-12-01")

## Step 2: Retrieve the data
get_data_time(query)
```

This results in the following:

    #> # A tibble: 5,704 × 9
    #>    date_value country_code country_name       confirmed deaths stringency_actual
    #>    <date>     <chr>        <chr>                  <int>  <int>             <dbl>
    #>  1 2022-12-01 ABW          Aruba                  43641    236             25.9 
    #>  2 2022-12-01 AFG          Afghanistan           206073   7834              2.78
    #>  3 2022-12-01 AGO          Angola                104676   1924             18.5 
    #>  4 2022-12-01 ALB          Albania               333360   3594             11.1 
    #>  5 2022-12-01 AND          Andorra                47219    157             11.1 
    #>  6 2022-12-01 ARE          United Arab Emira…   1044468   2348             13.9 
    #>  7 2022-12-01 ARG          Argentina            9727247 130025             25   
    #>  8 2022-12-01 AUS          Australia           10742127  16203             11.1 
    #>  9 2022-12-01 AUT          Austria              5566947  21216             35.2 
    #> 10 2022-12-01 AZE          Azerbaijan            824385   9981             45.4 
    #> # ℹ 5,694 more rows
    #> # ℹ 3 more variables: stringency <dbl>, stringency_legacy <dbl>,
    #> #   stringency_legacy_disp <dbl>

The `oxcgrt` functions are designed to work with pipe operators. The
steps shown above can be replicated using pipe operators as follows:

``` r
get_json_time(from = "2022-12-01") |>     ## Step 1: Creat API URL query
  get_data_time()                         ## Step 2: Retrieve data
```

This results in the same output as the earlier workflow albeit sorted
alphabetically by country code:

    #> # A tibble: 5,704 × 9
    #>    date_value country_code country_name       confirmed deaths stringency_actual
    #>    <date>     <chr>        <chr>                  <int>  <int>             <dbl>
    #>  1 2022-12-01 ABW          Aruba                  43641    236             25.9 
    #>  2 2022-12-01 AFG          Afghanistan           206073   7834              2.78
    #>  3 2022-12-01 AGO          Angola                104676   1924             18.5 
    #>  4 2022-12-01 ALB          Albania               333360   3594             11.1 
    #>  5 2022-12-01 AND          Andorra                47219    157             11.1 
    #>  6 2022-12-01 ARE          United Arab Emira…   1044468   2348             13.9 
    #>  7 2022-12-01 ARG          Argentina            9727247 130025             25   
    #>  8 2022-12-01 AUS          Australia           10742127  16203             11.1 
    #>  9 2022-12-01 AUT          Austria              5566947  21216             35.2 
    #> 10 2022-12-01 AZE          Azerbaijan            824385   9981             45.4 
    #> # ℹ 5,694 more rows
    #> # ℹ 3 more variables: stringency <dbl>, stringency_legacy <dbl>,
    #> #   stringency_legacy_disp <dbl>

For more detailed examples of how to retrieve data via the OxCGRT API
version 2, read [Retrieve data via OxCGRT
API](https://oxford-ihtm.io/oxcgrt/articles/retrieve.html).

### The `oxcgrt` calculate workflow

The `calculate_*` functions are based on the OxCGRT’s methodology
described
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/index_methodology.md).
There are two sets of calculate functions included in `oxcgrt`. The
first calculates the OxCGRT **sub-indices** and the second calculates
the four OxCGRT **indices** which are composed of various combinations
of the indicators used by OxCGRT sub-indices and indices.

For more detailed examples of how to calculate the various OxCGRT
sub-indices and indices, read [Calculate OxCGRT sub-indices and
indices](https://oxford-ihtm.io/oxcgrt/articles/calculate.html).

### Datasets

The `oxcgrt` package comes with helpful datasets which serve as guides
to facilitate in usage and interpretation of the OxCGRT data.

#### Codebook

The OxCGRT provides an authoritative codebook found
[here](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md).
The `oxcgrt` package has extracted the tables from this documentation
into a single codebook that can serve as a handy and convenient
reference for an R user when working with OxCGRT data in R. The OxCGRT
codebook can be accessed as follows:

``` r
codebook
```

which outputs the codebook as a singular table in `tbl` format as shown
below:

    #> # A tibble: 47 × 6
    #>    ID    Name                      Description Measurement Coding `Policy Group`
    #>    <chr> <chr>                     <chr>       <chr>       <chr>  <chr>         
    #>  1 C1    C1_School closing         "Record cl… Ordinal sc… 0 - n… Containment a…
    #>  2 C1    C1_Flag                   ""          Binary fla… 0 - t… Containment a…
    #>  3 C2    C2_Workplace closing      "Record cl… Ordinal sc… 0 - n… Containment a…
    #>  4 C2    C2_Flag                   ""          Binary fla… 0 - t… Containment a…
    #>  5 C3    C3_Cancel public events   "Record ca… Ordinal sc… 0 - n… Containment a…
    #>  6 C3    C3_Flag                   ""          Binary fla… 0 - t… Containment a…
    #>  7 C4    C4_Restrictions on gathe… "Record li… Ordinal sc… 0 - n… Containment a…
    #>  8 C4    C4_Flag                   ""          Binary fla… 0 - t… Containment a…
    #>  9 C5    C5_Close public transport "Record cl… Ordinal sc… 0 - n… Containment a…
    #> 10 C5    C5_Flag                   ""          Binary fla… 0 - t… Containment a…
    #> # ℹ 37 more rows

The current `oxcgrt` package version includes the OxCGRT **codebook
version 3.7** released on 11 March 2022.

#### Example OxCGRT indicators dataset

In the OxCGRT methodology document, an example indicator dataset is used
to demonstrate the calculation of per indicator sub-indices and the four
main indices that
[OxCGRT](https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker)
provides. This example dataset has been made available in table format
in the `oxcgrt` package and can be accessed as follows:

``` r
indicatorData
```

which outputs the example data as a singular table in `tbl` format as
shown below:

    #> # A tibble: 16 × 6
    #>    indicator value flag_value max_value  flag score
    #>    <chr>     <int>      <int>     <int> <int> <dbl>
    #>  1 C1            2          1         3     1  66.7
    #>  2 C2           NA         NA         3     1   0  
    #>  3 C3            2          0         2     1  75  
    #>  4 C4            2          0         4     1  37.5
    #>  5 C5            0         NA         2     1   0  
    #>  6 C6            1          0         3     1  16.7
    #>  7 C7            1          1         2     1  50  
    #>  8 C8            3         NA         4     0  75  
    #>  9 E1            2          0         2     1  75  
    #> 10 E2            2         NA         2     0 100  
    #> 11 H1            2          0         2     1  75  
    #> 12 H2            3         NA         3     0 100  
    #> 13 H3            2         NA         2     0 100  
    #> 14 H6            2          0         4     1  37.5
    #> 15 H7            2          1         5     1  40  
    #> 16 H8            2          1         3     1  66.7

This dataset is used by the `oxcgrt` package to test the `calculate_*`
functions and for demonstrating how these functions work. This dataset
can be useful for those trying to learn the OxCGRT’s calculation methods
and [R](https://cran.r-project.org) users who are learning how to use
the `oxcgrt` package `calculate_*` functions.

## Limitations

The current version of `oxcgrt` package is *experimental* in that its
stability and future development would depend on the OxCGRT’s current
and future development. The OxCGRT is in continuous evolution given that
the COVID-19 pandemic is still on-going and various governments’
responses to it are continuously changed and/or updated. The OxCGRT has
also been developing other indices that capture other aspects of
governments’ responses not yet covered by current indices.

The `oxcgrt` package author and maintainer commit to ensuring that
current functions are maintained and/or updated in a manner that ensures
backwards compatibility should changes to the data structure and/or to
the indices calculation are implemented by the OxCGRT team. This would
include maintaining the arguments used by the current functions,
maintaining the functionality of the current functions, and maintaining
the type of outputs of the current functions. Should changes implemented
by the OxCGRT team to the data structure and/or to the indices
calculation require the breaking of the syntax, functionality and/or
outputs of the current functions, a formal and proper deprecation
process will be implemented that include proper and detailed
documentation of the changes and the potential impact on current users.

## Disclaimer

The `oxcgrt` package is an independent development and is separate from
and not recognised and approved by the OxCGRT team. The author and
maintainer of the package is not affiliated with OxCGRT but is committed
to ensure fidelity to the methods and usage specified by OxCGRT and
accuracy of outputs described and required by OxCGRT.

Any mistakes, problems and issues with the functionality and outputs of
the `oxcgrt` including mistakes in interpretation of the calculation of
the sub-indices and indices noted (if any) are that of the author and
maintainer and not of the OxCGRT. Hence any problems and issues to the
usage, functionality and outputs of the `oxcgrt` package should be
addressed directly to the author and maintainer
[here](https://github.com/OxfordIHTM/oxcgrt/issues).

## Citation

When using the `oxcgrt` package, please cite both the source of the
OxCGRT data and `oxcgrt` package itself.

For the source of the OxCGRT data, the following citation is
recommended:

> Thomas Hale, Noam Angrist, Rafael Goldszmidt, Beatriz Kira, Anna
> Petherick, Toby Phillips, Samuel Webster, Emily Cameron-Blake, Laura
> Hallas, Saptarshi Majumdar, and Helen Tatlow. (2021). “A global panel
> database of pandemic policies (Oxford COVID-19 Government Response
> Tracker).” Nature Human Behaviour.
> <https://doi.org/10.1038/s41562-021-01079-8>

For the `oxcgrt` package, the suggested citation can be obtained using a
call to the `citation` function as follows:

``` r
citation("oxcgrt")
#> To cite oxcgrt in publications use:
#> 
#>   Guevarra E (2024). _oxcgrt: An Interface to the Oxford COVID-19
#>   Government Response Tracker API_. doi:10.5281/zenodo.4293648
#>   <https://doi.org/10.5281/zenodo.4293648>, R package version 0.2.0,
#>   <https://oxford-ihtm.io/oxcgrt/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {oxcgrt: An Interface to the Oxford COVID-19 Government Response Tracker API},
#>     author = {Ernest Guevarra},
#>     year = {2024},
#>     note = {R package version 0.2.0},
#>     url = {https://oxford-ihtm.io/oxcgrt/},
#>     doi = {10.5281/zenodo.4293648},
#>   }
```

## Community guidelines

Feedback, bug reports and feature requests are welcome; file issues or
seek support [here](https://github.com/OxfordIHTM/oxcgrt/issues). If you
would like to contribute to the package, please see our [contributing
guidelines](https://oxford-ihtm.io/oxcgrt/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://oxford-ihtm.io/oxcgrt/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
