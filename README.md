
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ecoharmonizeR

The goal of ecoharmonizeR is to …

## Installation

This is how the installation works:

``` r
devtools::install_github("datapumpernickel/ecoharmonizeR")
```

## Example

This is how you could download the same data from four different sources
(note that you need API Keys for some of these packages) and harmonize
them to be able to compare some of their values.

You need the dev version of comtradr to use the bulk download facility.

``` r
library(comRex)
library(comtradr)
library(Rat.las)

digits <- 2

download_dataverse_atlas(digits = digits, years = 2010)

atlas_data <- read_dataverse_atlas(digits = digits, years = 2010) |>
  mutate(classification_code = str_c("HS",digits))


comrex_data <- cr_get_data(
  freq = "A",
  reporter = "DE",
  partner = NULL,
  product = "27",
  indicators = NULL,
  time = 2010,
  flow = "1",
  update = F,
  verbose = T,
  ds_id = "045409"
)

comtrade_data <-ct_get_data(
  reporter = 'DEU',
  frequency = "A",
  partner = "everything",
  commodity_code = "27",
  start_date = 2010,
  end_date = 2010)

comtrade_bulk <-ct_get_bulk(
  reporter = 'DEU',
  start_date = 2010,
  end_date = 2010)


clean_atlas <-
  harmonize(atlas_data, source = "atlas") |>
  filter(cmd_code == "27",
         reporter_iso == "DEU",
         time == 2010,
         flow_desc == "import")

clean_comrex <-
  harmonize(comrex_data, source = "comex") |>
  filter(cmd_code == "27",
         reporter_iso == "DEU",
         time == 2010,
         flow_desc == "import")

clean_comtrade_data <-
  harmonize(comtrade_data, source = "comtrade") |>
  filter(cmd_code == "27",
         reporter_iso == "DEU",
         time == 2010,
         flow_desc == "import")

clean_comtrade_bulk <-
  harmonize(comtrade_bulk, source = "comtrade_bulk") |>
  filter(cmd_code == "27",
         reporter_iso == "DEU",
         time == 2010,
         flow_desc == "import")
```
