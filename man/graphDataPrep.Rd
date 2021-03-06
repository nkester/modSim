% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_step3_low_graphDataPrep.R
\name{graphDataPrep}
\alias{graphDataPrep}
\title{Prepare the Queried Data for Analysis}
\usage{
graphDataPrep(losMatViewData, acqMatViewData, sensor)
}
\arguments{
\item{losMatViewData}{A tibble resulting from the `low_queryData` function.}

\item{acqMatViewData}{A tibble resulting from the `low_queryData` function.}

\item{sensor}{A single character string for the sensor you want to structure
data for. For multiple sensors, execute this function multiple times.}
}
\value{
A four element named list of raw (by iteration) and aggregated (mean)
 data for line of sight and acquisition data.
}
\description{
This takes the results from `low_queryData` and returns expanded data for each
 time step both at the iteration level and also aggregated to the designPoint
 level.
}
\note{
Location: ./R/fct_step3_low_graphDataPrep.R

RMarkdown location: ./inst/step3_plotting/Step3_plotting.Rmd
}
\author{
Neil Kester, \email{nkester1@jhu.edu}
}
