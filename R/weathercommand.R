#' weathercommand: A package for calculating annual summary statistics of site-
#' specific climate data
#'
#' The weathercommand provides functions for two types of daily data: rainfall
#' and temperature. The resulting output from the functions are a variety of
#' summary statistics (mean, standard deviation, etc.).
#'
#' @section weathercommand functions:
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
#' The parallel versions are primarily just wrappers for the serial
#' implementations. A note of caution is warranted with the parallel processing
#' functions: these will happily attempt to consume as much RAM as they need.
#' Thus, if you are working with very large datasets ("large" being datasets
#' that have 10 million cells or more) on a not-so-new machine (pre-2018),
#' consider using the serial functions. This will take longer, but is less
#' likely to crash your well-meaning but not quite up to the task laptop from
#' six years ago.
#'
#' @section weathercommand data:
#'
#' @docType package
#' @name weathercommand
NULL
