#' Convert wide format weather data to long format
#'
#' @description \code{to_long} transforms wide-formatted data, where each
#' column represents an observation for a specific day for a specific site, to
#' long-formatted data, where each row corresponds to an observation for a
#' single day for a specific site.
#'
#' @param data       data.frame of wide format data
#' @param keep_cols  integer vector of columns in \code{data} to retain in long 
#' format; all other columns will be converted to key/value pairs in long 
#' format; defaults to 1. This should most likely correspond to the index of 
#' the column containing the site unique identifier
#' @param date_sep   character used to delimit variable prefix from date in
#' column names; defaults to underscore ("_")
#'
#' @details \code{to_long} assumes \code{data} has a column indicating site
#' unique identifier (i.e. a site ID column) and remaining columns are daily
#' measurements of a variable in question (i.e rainfall, temperature).
#' Remaining column names are assumed to have a prefix, a separator character,
#' and date in YYYYMMDD format. For example, rainfall data column names would
#' be something like "rf_19830325". Character strings occurring before
#' delimiter are not used. The character string in column names corresponding
#' to date are used to extract the date (YYYYMMDD).
#'
#' If \code{to_long} encounters invalid dates (e.g. 30 February 1984), such
#' dates will be assigned a value of \code{NA} in resultant data frame and a
#' warning will be thrown.
#'
#' @return data frame where each row corresponds to an observation for a single
#' day. Measurements are in the column \code{value} and date is in the column
#' \code{date}. Measurements retain the class of original data, and date is of
#' class \code{\link[base]{Date}}.
#'
#' @examples
#' \dontrun{
#' # to_long is not exported, so we use triple-colon (:::)
#' rain_long <- wxsumR:::to_long(data = rain_2yr)
#' }
#'
#' @import tidyr
#' @import dplyr
#' @importFrom lubridate as_date
to_long <- function(data, keep_cols = 1, date_sep = "_") {
  # Store names of columns to retain in output
  keep_col_names <- colnames(data)[keep_cols]

  # TODO: Still an opportunity for defensive programming with keep_cols and 
  # date_sep arguments
  
  # Convert to long
  long_data <- data %>%
    tidyr::pivot_longer(cols = -all_of(keep_cols),
                        names_to = "col_name",
                        values_to = "value")

  # Parse key column into date with lubridate's as_date
  long_data <- long_data %>%
    # Start by creating a new column called "datestring"
    tidyr::separate_wider_delim(cols = col_name, 
                                names = c(NA, "datestring"), 
                                delim = date_sep) %>%
    # Use as_date, specifying YYYYMMDD format
    dplyr::mutate(date = lubridate::as_date(x = datestring,
                                            format = "%Y%m%d")) %>%
    # Drop the year, month, day columns
    dplyr::select(dplyr::all_of(keep_col_names), date, value)
  
  # Warn users if any date funniness happens
  if (any(is.na(long_data$date))) {
    warning("Some columns could not be parsed into dates; this can occur if text parsing results in impossible dates, such as 30 February 1983")
  }

  return(long_data)
}
