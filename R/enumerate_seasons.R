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
#'
#' @return data frame with new columns, "season" and "season_year"
#' @import tidyverse
#' @import lubridate
enumerate_seasons <- function(data, start_month, end_month, day = 15) {
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

  # Year of first row serves as starting point
  current_season_year <- year(data$date[1])
  current_season_start <- as.Date(paste0(current_season_year, "-", start_month, "-", day), format = "%Y-%m-%d")

  # Need to see if there is a season in the starting year. If data do not start
  # on January 1, and the season includes dates that occur before the first date
  # of the date, then the starting_year should be incremented. For example, if
  # data start at September 1, 1983, and the season of interest is March 15 -
  # July 15, the first current_season_year will be 1984.
  if (data$date[1] > current_season_start) {
    current_season_year <- current_season_year + 1
    current_season_start <- as.Date(paste0(current_season_year, "-", start_month, "-", day), format = "%Y-%m-%d")
  }

  current_season_end <- as.Date(paste0(current_season_year, "-", end_month, "-", day), format = "%Y-%m-%d")

  # Need to make sure end is actually after start
  if (current_season_end < current_season_start) {
    current_season_end <- current_season_end + years(x = 1)
  }

  # Column that will ultimately hold the vector enumerating seasons
  data$season_year <- NA

  # Logical for easier processing of conditionals
  in_season <- FALSE

  # TODO: Flaw in logic somewhere?
  # Loop over rows
  #   Identify day of year
  #     is it before season end day of year?
  #         yes, is it after season start day of year?
  #             yes, mark as current season
  #             no, outside of season; mark as NA
  #         no, is it before the start of _next_ season?
  #             yes, mark as NA
  #             no, increment current season; mark as current season
  for(i in 1:nrow(data)) {
    # Extract the date of current row
    current_date <- data$date[i]
    # Season enumeration logic
    #    If the date is at or after the current season start AND at or before
    #    the current season start, update the season_year column and ensure
    #    in_season is true
    #    If not, and the in_season variable is set to TRUE, flip in_season to
    #    FALSE and increment current_season and current_season_year
    if (current_date >= current_season_start &&
        current_date <= current_season_end) {
      data$season_year[i] <- current_season_year
      # season_year[i] <- current_season_year
      in_season <- TRUE
    } else if (in_season && current_date > current_season_end) {
      # Increment the current_season_year
      current_season_year <- current_season_year + 1
      # Increment season years
      current_season_start <- current_season_start + years(x = 1)
      current_season_end <- current_season_end + years(x = 1)
      # Flip in_season to FALSE, so no more evaluations until we reach a date that
      # lies within a season
      in_season <- FALSE
    }
  }
  data <- na.omit(data)
  return(data)
}

#' Value for season year for observation
#'
#' @param x   vector of Date data
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param day          numeric day of month defining season (inclusive);
#' defaults to 15
#'
#' @return  integer vector of season year to which observation corresponds to
season_year <- function(x, start_month, end_month, day = 15) {
  sy <- NA # Will hold return value
  obs_year <- year(x)
  # Logic for extracting season year depends on whether or not season includes
  # the new year
  if (start_month > end_month) {
    # Season INCLUDES the new year
    # If season includes new year AND the date is 31 December

  } else {
    # Season EXCLUDES the new year
    start_date <- as.Date(paste0(obs_year), "-", start_month, "-", day)
    end_date <- as.Date(paste0(obs_year), "-", end_month, "-", day)
    if (x >= start_date & x <= end_date) {
      sy <- obs_year
    }
  }
  return(sy)
}
