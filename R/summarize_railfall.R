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
#' @param outputfile   path to output file
#'
#' @return NULL if outputfile is given, if outputfile is NULL, returns data
#' frame with rainfall summary statistics
#' @import tidyverse
summarize_rainfall <- function(inputfile, start_month, end_month,
                               start_day = 15, end_day = start_day,
                               rain_cutoff = 1, outputfile = "results_rain.csv",
                               na.rm = TRUE) {
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
    group_by(season_year, !!as.name(id_column_name)) %>%
    summarize(mean_season = mean(x = value, na.rm = na.rm),
              median_season = median(x = value, na.rm = na.rm),
              sd_season = sd(x = value, na.rm = na.rm),
              total_season = sum(x = value, na.rm = na.rm),
              skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
              norain = sum(x = value < rain_cutoff, na.rm = na.rm),
              raindays = sum(x = value >= rain_cutoff, na.rm = na.rm),
              raindays_percent = sum(x = value >= rain_cutoff, na.rm = na.rm)/n(),
              dry = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "mid"),
              dry_start = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "start"),
              dry_end = dry_interval(x = value, rain_cutoff = rain_cutoff, period = "end"))

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

#' Longest stretch of consecutive dry days
#'
#' @param x           numeric vector of rainfall measurements
#' @param rain_cutoff minimum amount of rainfall to count as non-dry day
#' @param period      period to measure longest dry spell; see \code{return}
#'
#' @return numeric vector of length 1 with the number of days that have rainfall
#' less than \code{rain_cutoff}; value returned is determined by value of
#' \code{period}:
#' \describe{
#'   \item{start}{number of consecutive days at beginning of season with less
#'   than \code{rain_cutoff} of measured rain; if first day of season had
#'   rainfall greater than or equal to \code{rain_cutoff}, the returned value
#'   will be zero}
#'   \item{mid}{longest stretch of days with less than \code{rain_cutoff}
#'   contained within the period; if rainfall was less than \code{rain_cutoff}
#'   for every day in defined season, the returned value will be zero}
#'   \item{end}{number of consecutive days at end of season with less
#'   than \code{rain_cutoff} of measured rain; if last day of season had
#'   rainfall greater than or equal to \code{rain_cutoff}, the returned value
#'   will be zero}
#' }
#'
#' @import stringr
dry_interval <- function(x, rain_cutoff = 1, period = c("start", "mid", "end")) {
  # A string that is a concatenation of 0s and 1s, where 0s are days where
  # rainfall is below rain_cutoff and 1s are days where rain is above
  # rain_cutoff
  rain_string <- paste0(as.integer(x >= rain_cutoff), collapse = "")

  # Split the string into a vector using 1 as delimiter; results in vector that
  # has empty character strings (previously had values of 1) and strings of
  # some number of consecutive 0s. Using stringr::str_split instead of
  # base::strsplit due to latter's undesired treatment of matches in final
  # position of string.
  # I don't know who developed this approach for the original STATA
  # implementation, but it kinda blew me away.
  rain_string_split <- unlist(stringr::str_split(string = rain_string,
                                pattern = "1"))

  longest <- 0
  if (period == "start") {
    longest <- nchar(rain_string_split)[1]
  } else if (period == "mid") {
    # If entire season was rain or non-rain, or if the season was characterized
    # by a single stretch of rain followed by consecutive non-rain days (or
    # vice-versa) should return 0
    if (length(rain_string_split) > 2) {
      longest <- max(nchar(rain_string_split[-c(1, length(rain_string_split))]))
    }
  } else if (period == "end") {
    longest <- nchar(rain_string_split)[length(rain_string_split)]
  }

  return(longest)
}
