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
#' @importFrom stringr str_split
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