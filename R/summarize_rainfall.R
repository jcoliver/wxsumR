# TODO: Given the wrapper nature of this function, would be good to add some
# defensive programming here
# File exists
# No February 29, or at least a warning

# TODO: Add parameter for site id (could assume column 1)

#' Provides rainfall summary statistics
#'
#' @param inputfile    path to csv file with daily rainfall measurement
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param start_day    numeric day of starting month defining season
#' (inclusive); defaults to 15
#' @param end_day      numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#' @param rain_cutoff  numeric minimum value for daily rainfall to be counted as
#' a rain day
#' @param na.rm        logical passed to summary statistic functions indicating 
#' treatment of \code{NA} values
#' @param wide         logical indicating whether or not to output as wide-
#' formatted data
#'
#' @return data frame with rainfall summary statistics
#'
#' @seealso \code{\link{summarize_temperature}} 
#' @export
#' @import dplyr
#' @importFrom stats median na.omit sd
#' @importFrom utils read.csv
summarize_rainfall <- function(inputfile, start_month, end_month,
                               start_day = 15, end_day = start_day,
                               rain_cutoff = 1, na.rm = TRUE, wide = TRUE) {
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
                                 start_day = start_day,
                                 end_day = end_day)

  # Assume first column has site id
  id_column_name <- colnames(rain_long)[1]

  # Start with calculating basic statistics, including the longest number of
  # consecutive days without rain in the period
  rain_summary <- rain_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_season = mean(x = value, na.rm = na.rm),
                     median_season = median(x = value, na.rm = na.rm),
                     sd_season = sd(x = value, na.rm = na.rm),
                     total_season = sum(x = value, na.rm = na.rm),
                     skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
                     norain = sum(x = value < rain_cutoff, na.rm = na.rm),
                     raindays = sum(x = value >= rain_cutoff, na.rm = na.rm),
                     raindays_percent = sum(x = value >= rain_cutoff, na.rm = na.rm)/dplyr::n(),
                     dry = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "mid"),
                     dry_start = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "start"),
                     dry_end = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "end"))
  
  # Add long-term values mean and standard-deviation values
  rain_summary <- dplyr::ungroup(rain_summary) %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(mean_period_total_season = mean(x = total_season),
                  sd_period_total_season = sd(x = total_season),
                  mean_period_norain = mean(x = norain),
                  sd_period_norain = sd(x = norain),
                  mean_period_raindays = mean(x = raindays),
                  sd_period_raindays = sd(x = raindays),
                  mean_period_raindays_percent = mean(x = raindays_percent),
                  sd_period_raindays_percent = sd(x = raindays_percent))
  
  # Finally, calculate deviations as deviations from the mean; for total_season,
  # also report as a z-score
  rain_summary <- dplyr::ungroup(rain_summary) %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::mutate(dev_total_season = total_season - mean_period_total_season,
                  z_total_season = (total_season - mean_period_total_season)/sd_period_total_season,
                  dev_raindays = raindays - mean_period_raindays,
                  dev_norain = norain - mean_period_norain,
                  dev_raindays_percent = raindays_percent - mean_period_raindays_percent)
  
  if (wide) {
    # Long-term columns won't be separated out into separate columns for each 
    # year
    long_term_cols <- c("mean_period_total_season", "sd_period_total_season", 
                        "mean_period_norain", "sd_period_norain", 
                        "mean_period_raindays", "sd_period_raindays", 
                        "mean_period_raindays_percent", "sd_period_raindays_percent")
    rain_summary <- wide_summary(x = rain_summary, 
                                 id_col = id_column_name, 
                                 long_term_cols = long_term_cols)
  }
  rain_summary <- as.data.frame(rain_summary)
  return(rain_summary)
}