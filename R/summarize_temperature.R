# TODO: Given the wrapper nature of this function, would be good to add some
# defensive programming here
# File exists
# No February 29, or at least a warning

# TODO: Add parameter for site id (could assume column 1)

#' Provides temperature summary statistics
#'
#' @param temperature   data frame with daily temperature data for each site
#' @param start_month   numeric starting month defining season (inclusive)
#' @param end_month     numeric ending month defining season (inclusive)
#' @param start_day     numeric day of starting month defining season
#' (inclusive)
#' @param end_day       numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#' @param growbase_low  numeric lower bound for calculating growing degree days
#' (inclusive)
#' @param growbase_high numeric upper bound for calculating growing degree days
#' (inclusive)
#' @param na.rm         logical passed to summary statistic functions indicating 
#' treatment of \code{NA} values
#' @param wide         logical indicating whether or not to output as wide-
#' formatted data
#'
#' @return data frame with temperature summary statistics
#' 
#' @seealso \code{\link{summarize_rainfall}} 
#' @export
#' @import dplyr
#' @importFrom stats median na.omit sd
#' @importFrom utils read.csv
summarize_temperature <- function(temperature, start_month, end_month,
                                  start_day = 15, end_day = start_day,
                                  growbase_low = 10, growbase_high = 20,
                                  na.rm = TRUE, wide = TRUE) {
  # Read in the data
  # temperature <- read.csv(file = inputfile)

  # Use to_long to convert to long format and parse column names into dates
  temperature_long <- to_long(data = temperature)

  # Exclude NA dates
  temperature_long <- temperature_long[!is.na(temperature_long$date), ]

  # Enumerate seasons
  temperature_long <- enumerate_seasons(data = temperature_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 start_day = start_day,
                                 end_day = end_day)

  # Assume first column has site id
  id_column_name <- colnames(temperature_long)[1]

  # Start with calculating basic statistics, including the growing degree days
  # for the season (Here defined as number of days in season that were within
  # the range defined by (growbase_low, growbase_high))
  temperature_summary <- temperature_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_season = mean(x = value, na.rm = na.rm),
                     median_season = median(x = value, na.rm = na.rm),
                     sd_season = sd(x = value, na.rm = na.rm),
                     skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
                     max_season = max(value, na.rm = na.rm),
                     gdd = sum(value >= growbase_low & value <= growbase_high),
                     tempbin20 = sum(value < quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[2]),
                     tempbin40 = sum(value >= quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[2] & 
                                       value < quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[3]),
                     tempbin60 = sum(value >= quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[3] &
                                       value < quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[4]),
                     tempbin80 = sum(value >= quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[4] &
                                       value < quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[5]),
                     tempbin100 = sum(value >= quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[5] &
                                        value <= quantile(x = value, probs = seq(from = 0, to = 1, by = 0.2))[6]))
  
  # Calculate seasonal average and standard deviation for gdd
  temperature_summary <- dplyr::ungroup(temperature_summary) %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(mean_gdd = mean(gdd),
                  sd_gdd = sd(gdd))
  
  # Finally, calculate deviations from mean for each season, measured as
  # difference from the mean and as z-score
  temperature_summary <- dplyr::ungroup(temperature_summary) %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::mutate(dev_gdd = gdd - mean_gdd,
                  z_gdd = (gdd - mean_gdd)/sd_gdd)
  
  if (wide) {
    # Long-term columns won't be separated out into separate columns for each 
    # year
    long_term_cols <- c("mean_gdd", "sd_gdd")
    temperature_summary <- wide_summary(x = temperature_summary, 
                                 id_col = id_column_name, 
                                 long_term_cols = long_term_cols)
  }
  temperature_summary <- as.data.frame(temperature_summary)
  return(temperature_summary)
}
