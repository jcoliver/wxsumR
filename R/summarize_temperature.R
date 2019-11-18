# TODO: Given the wrapper nature of this function, would be good to add some
# defensive programming here
# File exists
# No February 29, or at least a warning

# TODO: Add parameter for site id (could assume column 1)

#' Provides temperature summary statistics
#'
#' @param inputfile    path to csv file with daily temperature measurement
#' @param outputfile   path to output file
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive);
#'
#' @return NULL if outputfile is given, if outputfile is NULL, returns data
#' frame with temperature summary statistics
#' @import tidyverse
summarize_rainfall <- function(inputfile, start_month, end_month, day = 15,
                               growbase_low = 10, growbase_high = 20,
                               outputfile = "results_temp.csv", na.rm = TRUE) {
  # Read in the data
  temperature <- read.csv(file = inputfile)

  # Use to_long to convert to long format and parse column names into dates
  temperature_long <- to_long(data = temperature)

  # Exclude NA dates
  temperature_long <- temperature_long[!is.na(temperature_long$date), ]

  # Enumerate seasons
  temperature_long <- enumerate_seasons(data = temperature_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 day = day)

  # Assume first column has site id
  id_column_name <- colnames(temperature_long)[1]

  # Start with calculating basic statistics, including the growing degree days
  # for the season (Here defined as number of days in season that were within
  # the range defined by (growbase_low, growbase_high))
  temperature_summary <- temperature_long %>%
    group_by(season_year, !!as.name(id_column_name)) %>%
    summarize(mean_season = mean(x = value, na.rm = na.rm),
              median_season = median(x = value, na.rm = na.rm),
              sd_season = sd(x = value, na.rm = na.rm),
              skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
              max_season = max(value, na.rm = na.rm),
              gdd = sum(value >= growbase_low & value <= growbase_high))

  # Calculate seasonal average and standard deviation for gdd
  temperature_summary <- ungroup(temperature_summary) %>%
    group_by(!!as.name(id_column_name)) %>%
    mutate(mean_gdd = mean(gdd),
           sd_gdd = sd(gdd))

  # Finally, calculate deviations from mean for each season, measured as
  # difference from the mean and as z-score
  temperature_summary <- ungroup(temperature_summary) %>%
    group_by(season_year, !!as.name(id_column_name)) %>%
    mutate(dev_gdd = gdd - mean_gdd,
           z_gdd = (gdd - mean_gdd)/sd_gdd)

  return(temperature_summary)
}
