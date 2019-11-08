#' Provides rainfall summary statistics
#'
#' @param inputfile   path to csv file with daily rainfall measurement
#' @param outputfile  path to output file
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive); 
#' 
#' @return NULL if outputfile is given, if outputfile is NULL, returns data 
#' frame with rainfall summary statistics
#' @import tidyverse
summarize_rainfall <- function(inputfile, start_month, end_month, day = 15, 
                               outputfile = "results_rain.csv", na.rm = TRUE) {
  # TODO: Given the wrapper nature of this function, would be good to add some 
  # defensive programming here
  # File exists
  # No February 29, or at least a warning
  
  # Read in the data
  rain <- read.csv(file = inputfile)
  
  # Use to_long to convert to long format and parse column names into dates
  rain_long <- to_long(data = rain)
  
  # Enumerate seasons
  rain_long <- enumerate_seasons(data = rain_long, 
                                 start_month = start_month,
                                 end_month = end_month,
                                 day = day)
  
  # Do summary calculations for each season
  #    mean
  #    median
  #    sd
  #    total
  #    skew: (mean - median)/sd 
  #    days with < 1mm
  #    days with >= 1mm
  #    % days with >= 1mm
  #    longest consecutive period in season with < 1mm
  
  rain_summary <- rain_long %>%
    group_by(season_year) %>%
    summarize(mean_season = mean(x = value, na.rm = na.rm),
              median_season = median(x = value, na.rm = na.rm),
              sd_season = sd(x = value, na.rm = na.rm),
              total_season = sum(x = value, na.rm = na.rm),
              skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
              norain = sum(x = value < 1, na.rm = na.rm),
              raindays = sum(x = value >= 1, na.rm = na.rm),
              raindays_percent = sum(x = value >= 1, na.rm = na.rm)/n())
}