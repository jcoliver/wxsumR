#' Enumerates seasons in data frame
#' 
#' @param data         long-format data frame with weather data; ideally output 
#' of \code{toLong}
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive); 
#' defaults to 15
#' 
#' @return data frame with new columns, "season" and "season_year"
#' @import tidyverse
enumerate_seasons <- function(data, start_month, end_month, day = 15) {
  # TODO: any defensive checks for months and day?
  # Extract year, month, day
  
  # Season starts on a single day of the year, season ends on single day of year
  # Use ugly for loop to indicate season...
  # Loop over rows
  # Identify day of year
  # is it before season end day of year?
    # yes, is it after season start day of year?
      # yes, mark as current season
      # no, outside of season; mark as NA
    # no, is it before the start of _next_ season?
      # yes, mark as NA
      # no, increment current season; mark as current season
  
  # Drop all rows outside of season? If nothing else, could drop those months
  # that fall outside the seasonal window
}