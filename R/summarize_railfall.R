# TODO: Given the wrapper nature of this function, would be good to add some
# defensive programming here
# File exists
# No February 29, or at least a warning

# TODO: Add parameter for site id (could assume column 1)

#' Provides rainfall summary statistics
#'
#' @param inputfile    path to csv file with daily rainfall measurement
#' @param outputfile   path to output file
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive);
#'
#' @return NULL if outputfile is given, if outputfile is NULL, returns data
#' frame with rainfall summary statistics
#' @import tidyverse
summarize_rainfall <- function(inputfile, start_month, end_month, day = 15,
                               outputfile = "results_rain.csv", na.rm = TRUE) {
  # Read in the data
  rain <- read.csv(file = inputfile)

  # Use to_long to convert to long format and parse column names into dates
  rain_long <- to_long(data = rain)

  # Exclude NA dates
  rain_long <- rain_long[!is.na(rain_long$date), ]

  # Enumerate seasons
  rain_long <- enumerate_seasons(data = rain_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 day = day)

  # Assume first column has site id
  id_column_name <- colnames(rain_long)[1]

  # Start with calculating basic statistics
  rain_summary <- rain_long %>%
    group_by(season_year, !!as.name(id_column_name)) %>%
    summarize(mean_season = mean(x = value, na.rm = na.rm),
              median_season = median(x = value, na.rm = na.rm),
              sd_season = sd(x = value, na.rm = na.rm),
              total_season = sum(x = value, na.rm = na.rm),
              skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
              norain = sum(x = value < 1, na.rm = na.rm),
              raindays = sum(x = value >= 1, na.rm = na.rm),
              raindays_percent = sum(x = value >= 1, na.rm = na.rm)/n())

  # Add long-term values mean and standard-deviation values
  rain_summary <- ungroup(rain_summary) %>%
    group_by(!!as.name(id_column_name)) %>%
    mutate(mean_period_total_season = mean(x = total_season),
           sd_period_total_season = sd(x = total_season),
           mean_period_norain = mean(x = norain),
           sd_period_norain = sd(x = norain),
           mean_period_raindays = mean(x = raindays),
           sd_period_raindais = sd(x = raindays),
           mean_period_raindays_percent = mean(x = raindays_percent),
           sd_period_raindays_percent = sd(x = raindays_percent))

  # Finally, calculate deviations as deviations from the mean; for total_season,
  # also report as a z-score
  rain_summary <- ungroup(rain_summary) %>%
    group_by(season_year, !!as.name(id_column_name)) %>%
    mutate(dev_total_season = total_season - mean_period_total_season,
      z_total_season = (total_season - mean_period_total_season)/sd_period_total_season,
      dev_raindays = raindays - mean_period_raindays,
      dev_norain = norain - mean_period_norain,
      dev_raindays_percent = raindays_percent - mean_period_raindays_percent)

  return(rain_summary)
}
