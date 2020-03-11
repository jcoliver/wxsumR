#' Longest stretch of consecutive dry days
#'
#' @param x           numeric vector of rainfall measurements
#' @param rain_cutoff minimum amount of rainfall to count as non-dry day
#' @param period      period to measure longest dry spell; see \code{value}
#' @param na.rm       logical indicating treatment of \code{NA} values; by
#' default, \code{na.rm = TRUE}, \code{NA} values are ignored. If set to
#' \code{FALSE}, any \code{NA} values in \code{x} will result in a return value
#' of \code{NA}
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
#' @example
#' \donttest{
#'   rain <- c(0, 2, 3, 0, 0, 2, 0, 3, 0, 0, 0)
#'   # Longest interval at the end of season
#'   dry_test <- weathercommand:::dry_interval(x = rain, period = "end")
#'   dry_test
#'   # 3
#'
#'   # Longest interval in middle of season
#'   dry_test <- weathercommand:::dry_interval(x = rain, period = "mid")
#'   dry_test
#'   # 2
#'
#'   # Longest interval at start of season, considering days with rain of 2 or
#'   # lower (unitless) as dry days
#'   dry_test <- weathercommand:::dry_interval(x = rain, rain_cutoff = 2, period = "start")
#'   dry_test
#'   # 2
#' }
#' @importFrom stringr str_split str_c str_sub
dry_interval <- function(x, rain_cutoff = 1, period = c("start", "mid", "end"),
                         na.rm = TRUE) {

  # Vector of 0s and 1s, where 0s are days where rainfall is below rain_cutoff
  # and 1s are days where rain is above rain_cutoff
  rain_binary <- as.integer(x >= rain_cutoff)

  # If all values are NA, return NA
  if (all(is.na(rain_binary))) {
    return(NA)
  }

  # For the purposes of this calculation, if na.rm is TRUE, we need to
  # eliminate NA values from the rain_binary vector, but we cannot simply drop
  # them. That is, if we did, c(0, 0, NA, 0) becomes c(0, 0, 0). These two
  # resultant vectors have very different interpretations of the duration of the
  # longest dry periods. Instead, and for these purposes only, we replace NA
  # values with a "1" which is only indicating that the day was not a "dry" day
  if (na.rm) {
    rain_binary[is.na(rain_binary)] <- 1
  }

  # A string that is a concatenation of 0s and 1s, where 0s are days where
  # rainfall is below rain_cutoff and 1s are days where rain is above
  # rain_cutoff
  rain_string <- stringr::str_c(rain_binary, collapse = "")

  if (length(rain_string) == 0 | (length(rain_string) == 1 & is.na(rain_string))) {
    return(NA)
  }

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
    # Only return non-zero value for longest at start if first day was dry
    if (stringr::str_sub(string = rain_string, start = 1, end = 1) == 0) {
      longest <- nchar(rain_string_split)[1]
    }
  } else if (period == "mid") {
    # If entire season was rain or non-rain, or if the season was characterized
    # by a single stretch of rain followed by consecutive non-rain days (or
    # vice-versa) should return 0
    if (length(rain_string_split) > 2) {
      longest <- max(nchar(rain_string_split[-c(1, length(rain_string_split))]))
    }
  } else if (period == "end") {
    # Only return non-zero value for longest at start if last day was dry
    if (stringr::str_sub(string = rain_string, start = nchar(rain_string), end = nchar(rain_string)) == 0) {
      longest <- nchar(rain_string_split)[length(rain_string_split)]
    }
  }

  return(longest)
}
