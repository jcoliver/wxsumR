#' Enumerate seasons in data frame
#'
#' @description \code{enumerate_seasons} categorizes rows by an annual season
#' defined by \code{start_*} and \code{end_*} parameters. The resulting object
#' includes a \code{season_year} column indicating which season the date is
#' considered part of. This is generally only used for internal data
#' processing by \code{summarize_*} functions.
#'
#' @param data         long-format data frame with weather data including a
#' column \code{date}; ideally the data object is the result of a call to
#' \code{\link{to_long}}
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param start_day    numeric day of starting month defining season
#' (inclusive); defaults to 15
#' @param end_day      numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#'
#' @return A copy of the passed \code{data} data frame with an additional
#' column, \code{season_year} indicating the year of the season that the
#' observation "belongs" to. For seasons that \emph{do not} cross the new year,
#' this is simply the year the observation was made. For seasons that \emph{do}
#' include the new year (e.g. a season starts in November and ends in the
#' subsequent March), \code{season_year} is defined as the year in which the
#' season starts. That is, for a season spanning November through March, the
#' value of \code{season_year} for observations in January, February, and March
#' is the prior year (e.g. \code{season_year} for an observation on 1983-02-01
#' for a season spanning November through March would be 1984).
#'
#' @examples
#' \donttest{
#' df <- readRDS(file = "data/rain-small.Rds")
#' # convert to long format and parse column names into dates
#' rain_long <- to_long(data = df)
#' # enumerate seasons, defined by 30 November through 15 March
#' rain_long <- enumerate_seasons(data = df,
#'                                start_month = 11,
#'                                end_month = 3,
#'                                start_day = 30,
#'                                end_day = 15)
#' }
#'
#' @importFrom tidyr drop_na
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

  data <- tidyr::drop_na(data = data, season_year)
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
#' @return  integer vector of season year to which observation corresponds to.
#' For seasons that \emph{do not} cross the new year, this is simply the year
#' the observation was made. For seasons that \emph{do} include the new year
#' (e.g. a season starts in November and ends in the subsequent March),
#' \code{season_year} is defined as the year in which the season starts. That
#' is, for a season spanning November through March, the value of
#' \code{season_year} for observations in January, February, and March is the
#' prior year (e.g. \code{season_year} for an observation on 1983-02-01 for a
#' season spanning November through March would be 1984).
#'
#' @examples
#' \donttest{
#' dates <- as.Date(x = c("1983-12-15", "1984-03-01", "1984-12-15", "1985-03-01"))
#' # identify season year, where season is defined as 30 November through
#' # 15 March
#' sy <- season_year(x = dates,
#'                   start_month = 11,
#'                   end_month = 3,
#'                   start_day = 30,
#'                   end_day = 15)
#' }
#'
#' @importFrom lubridate year as_date
#' @importFrom stringr str_c
season_year <- function(x, start_month, end_month, start_day = 15,
                        end_day = start_day) {
  # Will hold return value (sy = season year)
  sy <- rep(NA, length(x))

  # The year of observation in x
  obs_year <- lubridate::year(x)

  # Starting and ending dates based on x's year
  start_OBSY <- lubridate::as_date(stringr::str_c(obs_year, "-", start_month, "-", start_day))
  end_OBSY <- lubridate::as_date(stringr::str_c(obs_year, "-", end_month, "-", end_day))

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
