# TODO: Could do a little more by checking for June 31, February 30?
# TODO: what about adding flexibility so day can be start_day and end_day
# TODO: Do checks to ensure date column is present in data

#' Enumerates seasons in data frame
#'
#' @param data         long-format data frame with weather data; ideally output
#' of \code{toLong}
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive);
#' defaults to 15
#' @param use_loop     use loop instead of vectorized algorithm
#'
#' @return data frame with new columns, "season" and "season_year"
#' @import tidyverse
#' @import lubridate
enumerate_seasons <- function(data, start_month, end_month, day = 15,
                              use_loop = FALSE) {
  if (start_month < 1 | start_month > 12) {
    stop(paste0("Invalid start_month (", start_month, ") passed to enumerate_seasons. Must be integer between 1 and 12"))
  }
  if (end_month < 1 | end_month > 12) {
    stop(paste0("Invalid end_month (", end_month, ") passed to enumerate_seasons. Must be integer between 1 and 12"))
  }
  if (day < 1 | day > 31) {
    stop(paste0("Invalid day (", day, ") passed to enumerate_seasons. Must be integer between 1 and 31"))
  }

  # Make sure data ordered by date
  data <- data[order(data$date), ]

  if (use_loop) {
    # Can drop months that are outside of the season, but need to check to see
    # if season includes new year
    includes_ny <- start_month > end_month

    # Logic for dropping rows depends on whether or not new year is in season
    if (includes_ny) {
      data <- data[month(data$date) >= start_month |
                     month(data$date) <= end_month, ]
    } else {
      data <- data[month(data$date) >= start_month &
                     month(data$date) <= end_month, ]
    }

    data$season_year <- NA
    for(i in 1:nrow(data)) {

      # Extract the date of current row
      current_date <- data$date[i]
      current_year <- year(current_date)

      start_date_OBSY <- as.Date(paste0(current_year, "-",
                                        start_month, "-", day))
      end_date_OBSY <- as.Date(paste0(current_year, "-",
                                      end_month, "-", day))

      if (start_month > end_month) {
        if (current_date >= start_date_OBSY & current_date >= end_date_OBSY) {
          data$season_year[i] <- current_year
        } else if (current_date < start_date_OBSY & current_date < end_date_OBSY) {
          data$season_year[i] <- current_year - 1
        }
      } else {
        if (current_date >= start_date_OBSY & current_date <= end_date_OBSY) {
          data$season_year[i] <- current_year
        }
      }
    }
  } else { # use vectorized approach instead
    data$season_year <- season_year(x = data$date,
                                    start_month = start_month,
                                    end_month = end_month,
                                    day = day)
  }

  data <- na.omit(data)
  return(data)
}

#' Value for season year for observation
#'
#' @param x            vector of Date data
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive);
#' defaults to 15
#'
#' @return  integer vector of season year to which observation corresponds to
season_year <- function(x, start_month, end_month, day = 15) {
  # Will hold return value
  sy <- rep(NA, length(x))

  # The year of observation in x
  obs_year <- year(x)

  # Starting and ending dates based on x's year
  start_OBSY <- as.Date(paste0(obs_year, "-", start_month, "-", day))
  end_OBSY <- as.Date(paste0(obs_year, "-", end_month, "-", day))

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
    sy[x < start_OBSY & x < end_OBSY] <- obs_year[x < start_OBSY & x < end_OBSY] - 1
  } else {
    # Season EXCLUDES the new year

    # If observation is after start in year of observation and before end in
    # year of observation, it is in season and season_year is the year of the
    # observation
    sy[x >= start_OBSY & x <= end_OBSY] <- obs_year[x >= start_OBSY & x <= end_OBSY]
  }
  return(sy)
}
