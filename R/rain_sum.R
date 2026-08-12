#' Rainfall summary statistics
#'
#' @description Uses daily site data to calculate summary rainfall statistics
#' for an annual season. This function aligns with the rainfall mode of the
#' Stata \code{wxsum} command.
#'
#' @details User-defined seasons will be, at most, one year long, defined by the
#' \code{start_*} and \code{end_*} parameters. Seasons \emph{can} span across
#' the new year, e.g. a season can start in November and end in March. Seasons
#' are enumerated by the year in which they start; i.e. if a season starts in
#' November and ends in March, the output for year YYYY will be based on the
#' data from November and December of YYYY and January, February, and March of
#' YYYY + 1.
#'
#' Deviations and z-scores are computed using a rolling window of
#' \code{lr_years} strictly preceding seasons. If fewer than \code{lr_years}
#' preceding seasons are available for a given year, deviations and z-scores
#' are \code{NA} for that season.
#'
#' By default, returns data in "wide" format, with a separate column for each
#' statistic for each year. If \code{wide = FALSE}, returns "long" format with
#' a \code{season_year} column.
#'
#' @param rain           data frame with daily rainfall data for each site
#' @param start_month    numeric starting month defining season (inclusive)
#' @param end_month      numeric ending month defining season (inclusive)
#' @param start_day      numeric day of starting month defining season
#' (inclusive); defaults to 1
#' @param end_day        numeric day of ending month defining season
#' (inclusive); defaults to \code{start_day}
#' @param rain_threshold numeric minimum value for daily rainfall to be counted
#' as a rain day; defaults to 1
#' @param lr_years       integer number of strictly preceding years used to
#' calculate rolling deviations and z-scores; defaults to 10
#' @param na_rm          logical passed to summary statistic functions
#' indicating treatment of \code{NA} values; defaults to \code{TRUE}
#' @param wide           logical indicating whether or not to output as wide-
#' formatted data; defaults to \code{TRUE}
#' @param id_index       integer column index of unique site id; defaults to 1
#' @param date_sep       character used to delimit variable prefix from date in
#' column names; defaults to underscore ("_")
#' @param complete_only logical indicating whether or not to include only years 
#' with complete data in output; defaults to \code{TRUE}. If \code{FALSE}, 
#' resulting output may include years that did not include start and/or end of 
#' defined season.
#'
#' @return tibble with rainfall summary statistics.
#' If \code{wide = FALSE}, returns values for each year for each site:
#' \describe{
#'   \item{mean}{Mean daily rainfall for the season}
#'   \item{median}{Median daily rainfall for the season}
#'   \item{sd}{Standard deviation of daily rainfall for the season}
#'   \item{skew}{Skew of daily rainfall for the season, where skew is
#'   defined by (mean - median)/sd}
#'   \item{mean_mo_total}{Mean of monthly rainfall totals within the
#'   season}
#'   \item{median_mo_total}{Median of monthly rainfall totals within the
#'   season}
#'   \item{sd_mo_total}{Standard deviation of monthly rainfall totals
#'   within the season}
#'   \item{skew_mo_total}{Skew of monthly rainfall totals within the
#'   season}
#'   \item{total}{Total rainfall over the season}
#'   \item{norain}{Number of days with rainfall less than
#'   \code{rain_threshold}}
#'   \item{raindays}{Number of days with rainfall greater than or equal to
#'   \code{rain_threshold}}
#'   \item{pct_raindays}{Share of observed days with rainfall greater than or
#'   equal to \code{rain_threshold}}
#'   \item{dry}{Longest stretch of consecutive days with rainfall less than
#'   \code{rain_threshold} contained within the season}
#'   \item{dev_total}{Deviation of total seasonal rainfall from the
#'   rolling mean of the preceding \code{lr_years} seasons}
#'   \item{z_total}{Z-score of total seasonal rainfall relative to
#'   the preceding \code{lr_years} seasons}
#'   \item{dev_raindays}{Deviation of rain day count from the rolling mean of
#'   the preceding \code{lr_years} seasons}
#'   \item{dev_norain}{Deviation of no-rain day count from the rolling mean of
#'   the preceding \code{lr_years} seasons}
#'   \item{dev_pct_raindays}{Deviation of rain day share from the rolling mean
#'   of the preceding \code{lr_years} seasons}
#' }
#'
#' If \code{wide = TRUE}, each season-specific statistic is represented by a
#' separate column for each year, with the year appended to the column name
#' (e.g. \code{mean_1997}, \code{total_1997}).
#'
#' @seealso \code{\link{summarize_rainfall}}
#'
#' @examples
#' \dontrun{
#' # Season defined by 1 May through 1 October
#' rain_summary <- rain_sum(
#'   rain = rain_2yr,
#'   start_month = 5,
#'   end_month = 10
#' )
#'
#' # Same season, long format output
#' rain_summary <- rain_sum(
#'   rain = rain_2yr,
#'   start_month = 5,
#'   end_month = 10,
#'   wide = FALSE
#' )
#'
#' # Season spanning the new year, 15 November through 15 March
#' rain_summary <- rain_sum(
#'   rain = rain_2yr,
#'   start_month = 11,
#'   end_month = 3,
#'   start_day = 15,
#'   end_day = 15
#' )
#' }
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom stats median sd
rain_sum <- function(rain, start_month, end_month,
                     start_day = 1, end_day = start_day,
                     rain_threshold = 1, lr_years = 10,
                     na_rm = TRUE, wide = TRUE,
                     id_index = 1, date_sep = "_", complete_only = TRUE) {
  # Extract name of column with site id
  id_column_name <- colnames(rain)[id_index]

  # Convert to long format and parse column names into dates
  # TODO: This is the time suck right here
  rain_long <- to_long(
    data = rain,
    keep_cols = id_index,
    date_sep = date_sep
  )

  # Exclude NA dates
  rain_long <- rain_long %>%
    tidyr::drop_na(date)

  # Enumerate seasons
  rain_long <- enumerate_seasons(
    data = rain_long,
    start_month = start_month,
    end_month = end_month,
    start_day = start_day,
    end_day = end_day
  )

  # Drop incomplete years (seasons) as appropriate
  if (complete_only) {
    # Find out which sites & season years have start day and end day
    have_start_end <- rain_long %>% 
      dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
      dplyr::filter((lubridate::month(date) == start_month & lubridate::day(date) == start_day) |
                      (lubridate::month(date) == end_month & lubridate::day(date) == end_day)) %>%
      dplyr::summarize(start_end_count = n(), .groups = "drop_last") %>%
      ungroup() %>%
      filter(start_end_count == 2) %>%
      select(-start_end_count)
    # Only keep site/season year combinations that have both start day and end 
    # day
    rain_long <- have_start_end %>%
      left_join(rain_long, by = join_by(season_year, !!as.name(id_column_name)))
  }
  
  # Daily-level statistics per season
  rain_summary <- rain_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(
      mean = mean(x = value, na.rm = na_rm),
      median = median(x = value, na.rm = na_rm),
      sd = sd(x = value, na.rm = na_rm),
      skew = (mean(x = value, na.rm = na_rm) -
        median(x = value, na.rm = na_rm)) /
        sd(x = value, na.rm = na_rm),
      total = sum(x = value, na.rm = na_rm),
      norain = sum(x = value < rain_threshold, na.rm = na_rm),
      raindays = sum(x = value >= rain_threshold, na.rm = na_rm),
      pct_raindays = sum(x = value >= rain_threshold, na.rm = na_rm) /
        dplyr::n(),
      dry = dry_interval(
        x = value, rain_cutoff = rain_threshold,
        period = "mid", na_rm = na_rm),
      dry_start = dry_interval(
        x = value, rain_cutoff = rain_threshold,
        period = "start", na_rm = na_rm),
      dry_end = dry_interval(
        x = value, rain_cutoff = rain_threshold,
        period = "end", na_rm = na_rm),
      .groups = "drop"
    )

  # Monthly-level statistics per season
  # Extract month from date for grouping
  rain_long$obs_month <- as.integer(format(rain_long$date, "%m"))

  monthly_totals <- rain_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name),obs_month) %>%
    dplyr::summarize(mo_total = sum(value, na.rm = na_rm), .groups = "drop")

  monthly_summary <- monthly_totals %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_mo_total = mean(mo_total, na.rm = na_rm),
                     median_mo_total = median(mo_total, na.rm = na_rm),
                     sd_mo_total = ifelse(dplyr::n() > 1,
                                          sd(mo_total, na.rm = na_rm),
                                          NA_real_),
                     skew_mo_total = ifelse(
                       dplyr::n() > 1,
                       (mean(mo_total, na.rm = na_rm) -
                          median(mo_total, na.rm = na_rm)) /
                         sd(mo_total, na.rm = na_rm),
                       NA_real_),
                     .groups = "drop"
    )
  
  # Join monthly summary into main summary
  rain_summary <- dplyr::left_join(rain_summary, monthly_summary,
    by = c("season_year", id_column_name)
  )

  # --- Rolling deviations and z-scores ---
  # Sort by site and season year so rolling window works correctly
  rain_summary <- dplyr::arrange(rain_summary,
                                 !!as.name(id_column_name),
                                 season_year
  )
  
  rain_summary <- rain_summary %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(
      z_total = .rolling_z(values = total,
                           years = season_year,
                           lr_years = lr_years),
      z_raindays = .rolling_z(values = raindays,
                              years = season_year,
                              lr_years = lr_years
      ),
      z_norain = .rolling_z(values = norain,
                            years = season_year,
                            lr_years = lr_years
      ),
      z_pct_raindays = .rolling_z(values = pct_raindays,
                                  years = season_year,
                                  lr_years = lr_years
      ),
      dev_total = .rolling_dev(values = total,
                               years = season_year,
                               lr_years = lr_years
      ),
      dev_raindays = .rolling_dev(values = raindays,
                                  years = season_year,
                                  lr_years = lr_years
      ),
      dev_norain = .rolling_dev(values = norain,
                                years = season_year,
                                lr_years = lr_years
      ),
      dev_pct_raindays = .rolling_dev(values = pct_raindays,
                                      years = season_year,
                                      lr_years = lr_years
      )
    ) %>%
    dplyr::ungroup()

  # --- Output format ---
  if (wide) {
    # All columns are year-specific with rolling deviations, so pivot all
    # non-id, non-year columns to wide format
    rain_summary <- rain_summary %>%
      tidyr::pivot_longer(-c(id_column_name, "season_year")) %>%
      tidyr::pivot_wider(
        id_cols = id_column_name,
        names_from = c(name, season_year),
        values_from = value,
        names_sep = "_"
      )
    # Need to end up removing those columns that have NA values across the
    # board for the deviation and z-score columns for the years that did not
    # have sufficient historical data (i.e. they had fewer than [lr_years]
    # years' of data before them).
    all_na <- apply(
      X = rain_summary,
      MARGIN = 2,
      FUN = function(x) {
        all(is.na(x))
      }
    )
    rain_summary <- rain_summary[, !all_na]
  }

  return(rain_summary)
}