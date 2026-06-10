#' Rolling deviation from long-run mean
#'
#' @description Computes the deviation of each value from the mean of the
#' preceding \code{lr_years} values, matched by year. Returns \code{NA} when
#' fewer than \code{lr_years} preceding years are available.
#'
#' @param values  numeric vector of the variable to compute deviations for
#' @param years   integer vector of season years corresponding to
#' \code{values}
#' @param lr_years integer number of preceding years in the rolling window
#'
#' @return numeric vector of deviations, same length as \code{values}
#'
#' @importFrom stats sd
#' @keywords internal
.rolling_dev <- function(values, years, lr_years) {
  n <- length(values)
  dev <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    target_years <- seq(years[i] - lr_years, years[i] - 1)
    window_idx <- which(years %in% target_years)
    if (length(window_idx) == lr_years) {
      dev[i] <- values[i] - mean(values[window_idx], na.rm = TRUE)
    }
  }
  dev
}


#' Rolling z-score from long-run mean
#'
#' @description Computes the z-score of each value relative to the mean and
#' standard deviation of the preceding \code{lr_years} values, matched by
#' year. Returns \code{NA} when fewer than \code{lr_years} preceding years
#' are available.
#'
#' @param values  numeric vector of the variable to compute z-scores for
#' @param years   integer vector of season years corresponding to
#' \code{values}
#' @param lr_years integer number of preceding years in the rolling window
#'
#' @return numeric vector of z-scores, same length as \code{values}
#'
#' @keywords internal
.rolling_z <- function(values, years, lr_years) {
  n <- length(values)
  z <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    target_years <- seq(years[i] - lr_years, years[i] - 1)
    window_idx <- which(years %in% target_years)
    if (length(window_idx) == lr_years) {
      z[i] <- (values[i] - mean(values[window_idx], na.rm = TRUE)) /
        sd(values[window_idx], na.rm = TRUE)
    }
  }
  z
}
