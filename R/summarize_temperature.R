#' Temperature summary statistics
#'
#' @description Uses daily site data to calculate summary temperature
#' statistics for an annual season. This function aligns with the temperature
#' mode of the Stata \code{wxsum} command.
#'
#' @details User-defined seasons will be, at most, one year long, defined by
#' the \code{start_*} and \code{end_*} parameters. Seasons \emph{can} span
#' across the new year, e.g. a season can start in November and end in March.
#' Seasons are enumerated by the year in which they start; i.e. if a season
#' starts in November and ends in March, the output for year YYYY will be
#' based on the data from November and December of YYYY and January,
#' February, and March of YYYY + 1.
#'
#' Growing degree days (GDD) are calculated using the continuous approach:
#' for each observed day, the contribution is
#' \code{min(max(value - growbase_low, 0), growbase_high - growbase_low)}.
#' GDD for the season is the sum of daily contributions.
#'
#' Killing degree days (KDD) are calculated as the cumulative sum of
#' degrees above \code{kdd_threshold}: for each observed day, the
#' contribution is \code{max(value - kdd_threshold, 0)}. KDD for the
#' season is the sum of daily contributions.
#'
#' Temperature bins are computed from within-season percentiles. With
#' \code{bins = 4}, the bin boundaries are at the 25th, 50th, and 75th
#' percentiles; with \code{bins = 5}, at the 20th, 40th, 60th, and 80th
#' percentiles, and so on. Each bin reports the share of observed days
#' falling within its range.
#'
#' Deviations and z-scores for GDD and KDD are computed using a rolling
#' window of \code{lr_years} strictly preceding seasons. If fewer than
#' \code{lr_years} preceding seasons are available for a given year,
#' deviations and z-scores are \code{NA} for that season.
#'
#' By default, returns data in "wide" format, with a separate column for
#' each statistic for each year. If \code{wide = FALSE}, returns "long"
#' format with a \code{season_year} column.
#'
#' @param temperature   data frame with daily temperature data for each site
#' @param start_month   numeric starting month defining season (inclusive)
#' @param end_month     numeric ending month defining season (inclusive)
#' @param start_day     numeric day of starting month defining season
#' (inclusive); defaults to 1
#' @param end_day       numeric day of ending month defining season
#' (inclusive); defaults to \code{start_day}
#' @param growbase_low  numeric lower bound for calculating growing degree
#' days (inclusive); defaults to 10
#' @param growbase_high numeric upper bound for calculating growing degree
#' days (inclusive); defaults to 30
#' @param kdd_threshold numeric threshold temperature for calculating
#' killing degree days; for each observed day, the contribution to KDD is
#' \code{max(value - kdd_threshold, 0)}
#' @param bins          integer number of temperature bins to compute from
#' within-season percentiles; must be between 1 and 42; defaults to 4
#' @param lr_years      integer number of strictly preceding years used to
#' calculate rolling deviations and z-scores; defaults to 10
#' @param na_rm         logical passed to summary statistic functions
#' indicating treatment of \code{NA} values; defaults to \code{TRUE}
#' @param wide          logical indicating whether or not to output as wide-
#' formatted data; defaults to \code{TRUE}
#' @param id_index      integer column index of unique site id; defaults
#' to 1
#' @param date_sep      character used to delimit variable prefix from date
#' in column names; defaults to underscore ("_")
#' @param complete_only logical indicating whether or not to include only years 
#' with complete data in output; defaults to \code{TRUE}. If \code{FALSE}, 
#' resulting output may include years that did not include start and/or end of 
#' defined season.
#'
#' @return tibble with temperature summary statistics.
#' If \code{wide = FALSE}, returns values for each year for each site:
#' \describe{
#'   \item{mean}{Mean daily temperature for the season}
#'   \item{median}{Median daily temperature for the season}
#'   \item{sd}{Standard deviation of daily temperature for the season}
#'   \item{skew}{Skew of daily temperature for the season, where skew is
#'   defined by (mean - median)/sd}
#'   \item{max}{Maximum daily temperature for the season}
#'   \item{gdd}{Growing degree days for the season, calculated as
#'   the sum of min(max(value - growbase_low, 0),
#'   growbase_high - growbase_low) across all observed days}
#'   \item{kdd}{Killing degree days for the season, calculated as
#'   the sum of max(value - kdd_threshold, 0) across all observed days}
#'   \item{tempbin_XX}{Share of observed days in temperature bin XX,
#'   where bins are defined by within-season percentiles and XX is a
#'   zero-padded bin number (e.g. tempbin_01, tempbin_02)}
#'   \item{binmean_XX}{Mean of temperature values for bin XX}
#'   \item{binsd_XX}{Standard deviation of temperature values for bin XX}
#'   \item{dev_gdd}{Deviation of GDD from the rolling mean of the
#'   preceding \code{lr_years} seasons}
#'   \item{z_gdd}{Z-score of GDD relative to the preceding
#'   \code{lr_years} seasons}
#'   \item{dev_kdd}{Deviation of KDD from the rolling mean of the
#'   preceding \code{lr_years} seasons}
#'   \item{z_kdd}{Z-score of KDD relative to the preceding
#'   \code{lr_years} seasons}
#' }
#'
#' If \code{wide = TRUE}, each season-specific statistic is represented
#' by a separate column for each year, with the year appended to the
#' column name (e.g. \code{mean_1997}, \code{gdd_1997}).
#'
#' @examples
#' \dontrun{
#' # Season defined by 1 March through 1 November
#' temp_summary <- summarize_temperature(
#'   temperature = temperature_2yr,
#'   start_month = 3,
#'   end_month = 11,
#'   kdd_threshold = 30
#' )
#'
#' # Same season, long format output
#' temp_summary <- summarize_temperature(
#'   temperature = temperature_2yr,
#'   start_month = 3,
#'   end_month = 11,
#'   kdd_threshold = 30,
#'   wide = FALSE
#' )
#'
#' # Season spanning the new year, 15 November through 15 March
#' temp_summary <- summarize_temperature(
#'   temperature = temperature_2yr,
#'   start_month = 11,
#'   end_month = 3,
#'   start_day = 15,
#'   end_day = 15,
#'   kdd_threshold = 30
#' )
#' }
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom stats median sd quantile
summarize_temperature <- function(temperature, start_month, end_month,
                     start_day = 1, end_day = start_day,
                     growbase_low = 10, growbase_high = 30,
                     kdd_threshold,
                     bins = 4, lr_years = 10,
                     na_rm = TRUE, wide = TRUE,
                     id_index = 1, date_sep = "_", complete_only = TRUE) {
  # Input validation
  if (growbase_high <= growbase_low) {
    stop("growbase_high (", growbase_high, ") must be greater than growbase_low (", growbase_low, ")")
  }
  if (!is.numeric(kdd_threshold) || length(kdd_threshold) != 1) {
    stop("kdd_threshold must be a single numeric value")
  }
  if (bins != floor(bins) || bins < 1 || bins > 42) {
    stop("bins must be an integer between 1 and 42; bins = ", bins)
  } else { # Make sure bins is really an integer (paranoid much?)
    bins <- as.integer(floor(bins))
  }
  
  # Extract name of column with site id
  id_column_name <- colnames(temperature)[id_index]
  
  # Convert to long format and parse column names into dates
  temp_long <- to_long(data = temperature,
                       keep_cols = id_index,
                       date_sep = date_sep)
  
  # Exclude NA dates
  temp_long <- temp_long |>
    tidyr::drop_na(date)
  
  # Enumerate seasons
  temp_long <- enumerate_seasons(data = temp_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 start_day = start_day,
                                 end_day = end_day)
  
  # Drop incomplete years (seasons) as appropriate
  if (complete_only) {
    # Find out which sites & season years have start day and end day
    have_start_end <- temp_long |> 
      dplyr::group_by(season_year, !!as.name(id_column_name)) |>
      dplyr::filter((lubridate::month(date) == start_month & lubridate::day(date) == start_day) |
                      (lubridate::month(date) == end_month & lubridate::day(date) == end_day)) |>
      dplyr::summarize(start_end_count = n(), .groups = "drop_last") |>
      dplyr::ungroup() |>
      dplyr::filter(start_end_count == 2) |>
      dplyr::select(-start_end_count)
    # Only keep site/season year combinations that have both start day and end 
    # day
    temp_long <- have_start_end |>
      dplyr::left_join(temp_long, 
                       by = join_by(season_year, !!as.name(id_column_name)))
  }
  
  # Daily-level statistics per season
  gdd_range <- growbase_high - growbase_low
  temp_summary <- temp_long |>
    dplyr::group_by(season_year, !!as.name(id_column_name)) |>
    dplyr::summarize(
      mean = mean(x = value, na.rm = na_rm),
      median = median(x = value, na.rm = na_rm),
      sd = sd(x = value, na.rm = na_rm),
      skew = (mean(x = value, na.rm = na_rm) -
                median(x = value, na.rm = na_rm)) /
        sd(x = value, na.rm = na_rm),
      max = ifelse(test = !all(is.na(value)),
                   yes = max(value, na.rm = na_rm),
                   no = NA),
      gdd = sum(pmin(pmax(value - growbase_low, 0, na.rm = na_rm), 
                     gdd_range, na.rm = na_rm),
                na.rm = na_rm),
      kdd = sum(pmax(value - kdd_threshold, 0), na.rm = na_rm),
      .groups = "drop")
  
  # Temperature bins per season
  # Compute bin shares separately and join, since the number of bin columns is 
  # dynamic based on the bins parameter
  temp_bins <- temp_long |>
    dplyr::group_by(season_year, !!as.name(id_column_name)) |>
    dplyr::summarize(.bins = list(.compute_temp_bins(values = value, 
                                                     bins = bins, 
                                                     na_rm = na_rm)),
                     .groups = "drop") |>
    tidyr::unnest_wider(.bins)
  
  temp_summary <- dplyr::left_join(temp_summary, temp_bins,
                                   by = c("season_year", id_column_name))
  
  # Rolling deviations and z-scores for GDD
  # Sort by site and season year so rolling window works correctly
  temp_summary <- dplyr::arrange(temp_summary, 
                                 !!as.name(id_column_name),
                                 season_year)
  
  temp_summary <- temp_summary |>
    dplyr::group_by(!!as.name(id_column_name)) |>
    dplyr::mutate(
      dev_gdd = .rolling_dev(values = gdd,
                             years = season_year,
                             lr_years = lr_years),
      z_gdd = .rolling_z(values = gdd,
                         years = season_year,
                         lr_years = lr_years),
      dev_kdd = .rolling_dev(values = kdd,
                             years = season_year,
                             lr_years = lr_years),
      z_kdd = .rolling_z(values = kdd,
                         years = season_year,
                         lr_years = lr_years)) |>
    dplyr::ungroup()
  
  # Do summary stats calculations for temperature bins, using join *after* the 
  # (potential) pivot to wide format
  bin_stats <- temp_summary |>
    dplyr::group_by(!!as.name(id_column_name)) |>
    dplyr::summarize(across(.cols = starts_with("tempbin_"),
                            .fns = list(mean = ~ mean(.x, na.rm = na_rm),
                                        sd = ~ sd(.x, na.rm = na_rm)),
                            .names = "bin{.fn}_{.col}")) |> 
    dplyr::ungroup() |>
    dplyr::select(all_of(id_column_name), contains("_tempbin_")) |>
    # Need to rename these columns by removing "tempbin_"
    dplyr::rename_with(.fn = ~ gsub(pattern = "_tempbin_",
                                    replacement = "_",
                                    x = .x),
                       .cols = contains("_tempbin_"))
  
  # --- Output format ---
  if (wide) {
    # After pivoting, we can remove deviation and skew columns for those years 
    # with insufficient previous data. That is, given the window of lr_years, 
    # dev_ and z_ columns of years that are < start_year + lr_years can be 
    # dropped. But we need year information before the pivot happens.
    first_year <- min(temp_summary$season_year)
    dev_z_start <- first_year  + lr_years
    
    # Pivot all non-id, non-year columns to wide format
    temp_summary <- temp_summary |>
      tidyr::pivot_longer(-dplyr::all_of(c(id_column_name, "season_year"))) |>
      tidyr::pivot_wider(id_cols = dplyr::all_of(id_column_name),
                         names_from = c(name, season_year),
                         values_from = value,
                         names_sep = "_")

    # Need to end up removing those columns that have NA values across the
    # board for the deviation and z-score columns for the years that did not
    # have sufficient historical data (i.e. they had fewer than [lr_years]
    # years' of data before them).
    all_na <- apply(
      X = temp_summary,
      MARGIN = 2,
      FUN = function(x) {
        all(is.na(x))
      }
    )
    temp_summary <- temp_summary[, !all_na]
  }
  
  # After pivoting (or not) add in the summary stats for the bin-level summary 
  # statistics
  temp_summary <- temp_summary |>
    left_join(bin_stats, by = id_column_name)
  
  return(temp_summary)
}

#' Compute temperature bin shares for a single season-site group
#'
#' @description Computes the share of observed days falling into each of
#' \code{bins} percentile-based temperature bins. Bin boundaries are
#' determined by within-group quantiles.
#'
#' @param values numeric vector of daily temperature values
#' @param bins   integer number of bins
#' @param na_rm  logical indicating treatment of \code{NA} values
#'
#' @return named list with one element per bin (tempbin_01, tempbin_02,
#' etc.), each containing the share of observed days in that bin
#'
#' @keywords internal
.compute_temp_bins <- function(values, bins, na_rm) {
  # Drop missing, if appropriate
  observed <- if (na_rm){ 
    values[!is.na(values)] 
  } else {
    values
  }
  n_obs <- length(observed)
  
  result <- stats::setNames(object = as.list(rep(NA_real_, bins)),
                            nm = sprintf("tempbin_%02d", seq_len(bins)))
  
  if (n_obs < bins) {
    return(result)
  }
  
  # Compute quantile boundaries
  probs <- seq_len(bins - 1) / bins
  boundaries <- stats::quantile(observed, probs = probs, na.rm = na_rm)
  
  for (b in seq_len(bins)) {
    bin_name <- sprintf("tempbin_%02d", b)
    if (b == 1) {
      result[[bin_name]] <- sum(observed < boundaries[1]) / n_obs
    } else if (b == bins) {
      result[[bin_name]] <- sum(
        observed >= boundaries[bins - 1]
      ) / n_obs
    } else {
      result[[bin_name]] <- sum(
        observed >= boundaries[b - 1] & observed < boundaries[b]
      ) / n_obs
    }
  }
  return(result)
}
