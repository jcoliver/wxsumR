#' Convert wide format weather data to long format
#' 
#' @param data       wide format data
#' @param keep_cols  columns in \code{data} to retain in long format; all other 
#' columns will be converted to key/value pairs in long format; defaults to 1
#' @param date_sep   character used to delimit variable prefix from date in 
#' column names; defaults to underscore ("_")
#' 
#' @return data frame where each row corresponds to an observation for a single 
#' day
#' 
#' @import tidyr
#' @import dplyr
#' @importFrom magrittr %>%
to_long <- function(data, keep_cols = 1, date_sep = "_") {
  # Store names of columns to retain in output
  keep_col_names <- colnames(data)[keep_cols]
  
  # Convert to long
  # TODO: Need to check for cases when keep_cols length !=1
  # TODO: What if there is not date separator?
  long_data <- data %>%
    tidyr::pivot_longer(names_to = "col_name", 
                        values_to = "value", 
                        -keep_cols)
  
  # Parse key column into year, month, day
  long_data <- long_data %>%
    # Start by creating a new column called "date"
    tidyr::separate(col = col_name, into = c(NA, "date"), sep = date_sep) %>%
    # Parse out the separate parts of the date
    dplyr::mutate(year = substr(x = date, start = 1, stop = 4),
                  month = substr(x = date, start = 5, stop = 6),
                  day = substr(x = date, start = 7, stop = 8)) %>%
    # Make a single column for Date
    dplyr::mutate(date = lubridate::as_date(stringr::str_c(year, "-", month, "-", day))) %>%
    # Drop the year, month, day columns
    dplyr::select(keep_col_names, date, value)

  # Warn users if any date funniness happens
  if (any(is.na(long_data$date))) {
    warning("Some columns could not be parsed into dates; this can occur if text parsing results in impossible dates, such as 30 February 1983")
  }
  
  return(long_data)
}
