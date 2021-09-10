#' wxsumR: A package for calculating annual summary statistics of site-
#' specific climate data
#'
#' The wxsumR package provides functions for two types of daily data:
#' rainfall and temperature. The resulting output from the functions are a
#' variety of summary statistics (mean, standard deviation, etc.). See
#' documentation for each function for full details of the statistics being
#' calculated.
#'
#' @section wxsumR functions:
#' The two primary functions are:
#' \itemize{
#'   \item \code{\link{summarize_rainfall}}
#'   \item \code{\link{summarize_temperature}}
#' }
#'
#' Each of the two functions has a corresponding parallel implementation, which
#' use functions in the parallel package:
#' \itemize{
#'   \item \code{\link{par_summarize_rainfall}}
#'   \item \code{\link{par_summarize_temperature}}
#' }
#'
#' The parallel versions are primarily just wrappers for the serial
#' implementations. A note of caution is warranted with the parallel processing
#' functions: these will happily attempt to consume as much RAM as they need.
#' Thus, if you are working with very large datasets ("large" being datasets
#' that have 10 million cells or more) on a not-so-new machine (pre-2018),
#' consider using the serial functions. This will take longer, but is less
#' likely to crash your well-meaning but not quite up to the task laptop from
#' six years ago. One example of how this can be implemented can be found in the
#' wxsumR Quick Reference vignette.
#'
#' @section wxsumR data:
#' Sample datasets for both rainfall and temperature are provided as examples.
#'
#' These datasets illustrate the expected format of input as well as column
#' naming conventions. As column names are expected to include information (i.e.
#' the date of an observation), it is imperative that this format is used for
#' all input files. For more information about expected input column formats,
#' see documentation for \code{date_sep} parameter in
#' \code{\link{summarize_rainfall}} or \code{\link{summarize_temperature}}.
#'
#' @docType package
#' @name wxsumR
NULL
