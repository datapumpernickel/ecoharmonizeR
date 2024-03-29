% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonize.R
\name{harmonize}
\alias{harmonize}
\title{Harmonize Trade Data from Multiple Sources}
\usage{
harmonize(data, source)
}
\arguments{
\item{data}{The trade data frame to be harmonized.}

\item{source}{A character string specifying the source of the data.
Valid options are "comex", "atlas", "comtrade", and "comtrade_bulk".}
}
\value{
A harmonized and cleaned data frame ready for analysis.
The returned data frame includes standardized columns for frequency, reporter description,
reporter ISO code, partner description, partner ISO code, product description, product code,
flow description, time period, and the primary value of trade. Additionally, a source column
is added to indicate the origin of the data.
}
\description{
This function harmonizes trade data from various sources including COMEX, Atlas, and COMTRADE datasets.
It standardizes the dataset structure, cleans, and prepares it for analysis by applying specific
transformations based on the source of the data.
}
\examples{
if (interactive()) {
  # Harmonize data from COMEX
  harmonized_comex <- harmonize(comex_data, "comex")

  # Harmonize data from Atlas
  harmonized_atlas <- harmonize(atlas_data, "atlas")

  # Harmonize data from COMTRADE
  harmonized_comtrade <- harmonize(comtrade_data, "comtrade")

  # Harmonize bulk data from COMTRADE
  harmonized_comtrade_bulk <- harmonize(comtrade_bulk_data, "comtrade_bulk")
}

}
