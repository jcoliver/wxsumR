# TODO: Could do a little more by checking for June 31, February 30?

# TODO: Do checks to ensure date column is present in data

#' Enumerates seasons in data frame
#'
#' @param data         long-format data frame with weather data; ideally output
#' of \code{toLong}
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param start_day    numeric day of starting month defining season
#' (inclusive); defaults to 15
#' @param end_day      numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#'
#' @return data frame with new columns, "season" and "season_year"
#' @import tidyverse
#' @import lubridate
enumerate_seasons <- function(data, start_month, end_month, start_day = 15,
                              end_day = start_day) {
  if (start_month < 1 | start_month > 12) {
    stop(paste0("Invalid start_month (", start_month, ") passed to enumerate_seasons. Must be integer between 1 and 12"))
  }
  if (end_month < 1 | end_month > 12) {
    stop(paste0("Invalid end_month (", end_month, ") passed to enumerate_seasons. Must be integer between 1 and 12"))
  }
  if (start_day < 1 | start_day > 31) {
    stop(paste0("Invalid start day (", start_day, ") passed to enumerate_seasons. Must be integer between 1 and 31"))
  }
  if (end_day < 1 | end_day > 31) {
    stop(paste0("Invalid end day (", end_day, ") passed to enumerate_seasons. Must be integer between 1 and 31"))
  }

  data$season_year <- season_year(x = data$date,
                                  start_month = start_month,
                                  end_month = end_month,
                                  start_day = start_day,
                                  end_day = end_day)

  data <- na.omit(data)
  return(data)
}

#' Value for season year for observation
#'
#' @param x            vector of Date data
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param start_day    numeric day of starting month defining season
#' (inclusive); defaults to 15
#' @param end_day      numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#'
#' @return  integer vector of season year to which observation corresponds to
season_year <- function(x, start_month, end_month, start_day = 15,
                        end_day = start_day) {
  # Will hold return value
  sy <- rep(NA, length(x))

  # The year of observation in x
  obs_year <- year(x)

  # Starting and ending dates based on x's year
  start_OBSY <- as.Date(paste0(obs_year, "-", start_month, "-", start_day))
  end_OBSY <- as.Date(paste0(obs_year, "-", end_month, "-", end_day))

  # Logic for extracting season year depends on whether or not season includes
  # the new year
  if (start_month > end_month) {
    # Season INCLUDES the new year

    # If x occurs after both start and end for year of observation, it is in
    # season and its season_year is just the year of the observation
    sy[x >= start_OBSY & x >= end_OBSY] <- obs_year[x >= start_OBSY & x >= end_OBSY]

    # If x occurs before both start and end for the year of observation, it is
    # in season and its season_year is the year *prior* to the year of the
    # observation
    sy[x < start_OBSY & x <= end_OBSY] <- obs_year[x < start_OBSY & x <= end_OBSY] - 1
  } else {
    # Season EXCLUDES the new year

    # If observation is after start in year of observation and before end in
    # year of observation, it is in season and season_year is the year of the
    # observation
    sy[x >= start_OBSY & x <= end_OBSY] <- obs_year[x >= start_OBSY & x <= end_OBSY]
  }
  return(sy)
}
