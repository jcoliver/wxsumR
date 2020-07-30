#' Rainfall summary statistics
#'
#' @description Uses daily site data to calculate summary rainfall statistics
#' for an annual season.
#'
#' @details User-defined seasons will be, at most, one year long, defined by the
#' \code{start_*} and \code{end_*} parameters. Seasons \emph{can} span across
#' the new year, e.g. a season can start in November and end in March. Seasons
#' are enumerated by the year in which they start; i.e. if a season starts in
#' November and ends in March, the output for year YYYY will be based on the
#' data from November and December of YYYY and January, February, and March of
#' YYYY + 1.
#'
#' By default, will return data in "long" format, with a column indicating the
#' year the data correspond to (but see discussion of season enumeration above).
#' If \code{wide = TRUE}, output will include a separate column for each
#' statistic for each year (see \strong{Value}). For example, if \code{wide =
#' FALSE} and the data include daily measurements from 1997 to 2002, the output
#' will have a column \code{year} and a column \code{mean_season}. For these
#' same data, if \code{wide = TRUE}, there will be no \code{year} column, but
#' instead it will contain columns \code{mean_season_1997},
#' \code{mean_season_1998}...\code{mean_season_2002}.
#'
#' @param rain         data frame with daily rainfall data for each site
#' @param start_month  numeric starting month defining season (inclusive)
#' @param end_month    numeric ending month defining season (inclusive)
#' @param start_day    numeric day of starting month defining season
#' (inclusive); defaults to 15
#' @param end_day      numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#' @param rain_cutoff  numeric minimum value for daily rainfall to be counted as
#' a rain day
#' @param na.rm        logical passed to summary statistic functions indicating
#' treatment of \code{NA} values
#' @param wide         logical indicating whether or not to output as wide-
#' formatted data
#' @param id_index     integer column index of unique site id
#' @param date_sep     character used to delimit variable prefix from date in
#' column names; defaults to underscore ("_")
#'
#' @return tibble with rainfall summary statistics
#' If \code{wide = FALSE}, returns values for each year for each site:
#' \describe{
#'   \item{mean_season}{Mean rainfall for the season}
#'   \item{median_season}{Median rainfall for the season}
#'   \item{sd_season}{Standard deviation of rainfall for the season}
#'   \item{total_season}{Total rainfall rainfall over the season}
#'   \item{skew_season}{Skew of rainfall for the season, where skew is defined
#'   by (mean - median)/sd}
#'   \item{norain}{Total number of days with rain less than \code{rain_cutoff}}
#'   \item{raindays}{Total number of days with rain greater than or equal to
#'   \code{rain_cutoff}}
#'   \item{percent_raindays}{Percentage of days in season with rain greater than
#'   or equal to \code{rain_cutoff}}
#'   \item{dry}{Longest stretch of days with less than \code{rain_cutoff}
#'   contained within the period; if rainfall was less than \code{rain_cutoff}
#'   for every day in defined season, the returned value will be zero}
#'   \item{dry_start}{Number of consecutive days at beginning of season with
#'   less than \code{rain_cutoff} of measured rain; if first day of season had
#'   rainfall greater than or equal to \code{rain_cutoff}, the returned value
#'   will be zero}
#'   \item{dry_end}{Number of consecutive days at end of season with less
#'   than \code{rain_cutoff} of measured rain; if last day of season had
#'   rainfall greater than or equal to \code{rain_cutoff}, the returned value
#'   will be zero}
#'   \item{mean_period_total_season}{Mean total seasonal rainfall across all
#'   seasons}
#'   \item{sd_period_total_season}{Standard deviation of total rainfall across
#'   all seasons}
#'   \item{mean_period_norain}{Mean number of days with rainfall less than
#'   \code{rain_cutoff} across all seasons}
#'   \item{sd_period_norain}{Standard deviation of number of days with rainfall
#'   less than \code{rain_cutoff} across all seasons}
#'   \item{mean_period_raindays}{Mean number of days with rainfall greater than
#'   or equal to \code{rain_cutoff} across all seasons}
#'   \item{sd_period_norain}{Standard deviation of number of days with rainfall
#'   greater than or equal to \code{rain_cutoff} across all seasons}
#'   \item{mean_period_percent_raindays}{Mean percentage of days in season with
#'   rain greater than or equal to \code{rain_cutoff} across all seasons}
#'   \item{sd_period_percent_raindays}{Standard deviation of percentage of days
#'   in season with rain greater than or equal to \code{rain_cutoff} across all
#'   seasons}
#'   \item{dev_total_season}{Amount by which season total rainfall deviates from
#'   the mean total rainfall across seasons}
#'   \item{z_total_season}{Difference of total seasonal rainfall and mean of
#'   total rainfall across seasons, divided by \code{sd_period_total_season}}
#'   \item{dev_raindays}{Difference in number of days with rainfall greater than
#'   or equal to \code{rain_cutoff} from the mean number of days with rainfall
#'   greater than or equal to \code{rain_cutoff} across seasons}
#'   \item{dev_norain}{Difference in number of days with rainfall less than
#'   \code{rain_cutoff} from the mean number of days with rainfall less than
#'   \code{rain_cutoff} across seasons}
#'   \item{dev_percent_raindays}{Difference in percentage of days with rainfall
#'   greater than or equal to \code{rain_cutoff} from the percentage of days
#'   with rainfall greater than or equal to \code{rain_cutoff} across seasons}
#'   \item{z_percent_raindays}{Difference in percentage of days with rainfall
#'   greater than or equal to \code{rain_cutoff} from
#'   \code{mean_period_percent_raindays}, divided by
#'   \code{sd_period_percent_raindays}}
#' }
#'
#' If \code{wide = TRUE}, all columns except those with *_period_* pattern are
#' replaced with one column for each year. For example, if the data include
#' daily measurements from 1997 to 2002, there will be no \code{mean_season}
#' column in the output, but will instead have columns \code{mean_season_1997},
#' \code{mean_season_1998}...\code{mean_season_2002}.
#'
#' @seealso \code{\link{summarize_temperature}}, \code{\link{par_summarize_rainfall}}
#'
#' @examples
#' \dontrun{
#' # Season defined by 15 March through 15 November
#' rain_summary <- summarize_rainfall(rain = rain_2yr,
#'                                    start_month = 3,
#'                                    end_month = 11)
#'
#' # As example above, but output in "long" format
#' rain_summary <- summarize_rainfall(rain = rain_2yr,
#'                                    start_month = 3,
#'                                    end_month = 11,
#'                                    wide = FALSE)
#'
#' # Season defined by 30 November through 15 March
#' rain_summary <- summarize_rainfall(rain = rain_2yr,
#'                                    start_month = 11,
#'                                    end_month = 3,
#'                                    start_day = 30,
#'                                    end_day = 15)
#' }
#'
#' @export
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#' @importFrom stats median na.omit sd
#' @importFrom utils read.csv
summarize_rainfall <- function(rain, start_month, end_month,
                               start_day = 15, end_day = start_day,
                               rain_cutoff = 1, na.rm = TRUE, wide = TRUE,
                               id_index = 1, date_sep = "_") {

  # Extract name of column with site id
  id_column_name <- colnames(rain)[id_index]

  # Use to_long to convert to long format and parse column names into dates
  rain_long <- to_long(data = rain,
                       keep_cols = id_index,
                       date_sep = date_sep)

  # Exclude NA dates
  rain_long <- rain_long %>%
    tidyr::drop_na(date)

  # Enumerate seasons
  rain_long <- enumerate_seasons(data = rain_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 start_day = start_day,
                                 end_day = end_day)

  # Start with calculating basic statistics, including the longest number of
  # consecutive days without rain in the period
  rain_summary <- rain_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_season = mean(x = .data$value, na.rm = na.rm),
                     median_season = median(x = .data$value, na.rm = na.rm),
                     sd_season = sd(x = .data$value, na.rm = na.rm),
                     total_season = sum(x = .data$value, na.rm = na.rm),
                     skew_season = (mean(x = .data$value, na.rm = na.rm) - median(x = .data$value, na.rm = na.rm))/sd(x = .data$value, na.rm = na.rm),
                     norain = sum(x = .data$value < rain_cutoff, na.rm = na.rm),
                     raindays = sum(x = .data$value >= rain_cutoff, na.rm = na.rm),
                     percent_raindays = sum(x = .data$value >= rain_cutoff, na.rm = na.rm)/dplyr::n(),
                     dry = dry_interval(x = .data$value, rain_cutoff = rain_cutoff, period = "mid", na.rm = na.rm),
                     dry_start = dry_interval(x = .data$value, rain_cutoff = rain_cutoff, period = "start", na.rm = na.rm),
                     dry_end = dry_interval(x = .data$value, rain_cutoff = rain_cutoff, period = "end", na.rm = na.rm))

  # Add long-term values mean and standard-deviation values
  rain_summary <- dplyr::ungroup(rain_summary) %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(mean_period_total_season = mean(x = .data$total_season),
                  sd_period_total_season = sd(x = .data$total_season),
                  mean_period_norain = mean(x = .data$norain),
                  sd_period_norain = sd(x = .data$norain),
                  mean_period_raindays = mean(x = .data$raindays),
                  sd_period_raindays = sd(x = .data$raindays),
                  mean_period_percent_raindays = mean(x = .data$percent_raindays),
                  sd_period_percent_raindays = sd(x = .data$percent_raindays))

  # Finally, calculate deviations as deviations from the mean; for total_season,
  # also report as a z-score
  rain_summary <- dplyr::ungroup(rain_summary) %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::mutate(dev_total_season = .data$total_season - .data$mean_period_total_season,
                  z_total_season = (.data$total_season - .data$mean_period_total_season)/.data$sd_period_total_season,
                  dev_raindays = .data$raindays - .data$mean_period_raindays,
                  dev_norain = .data$norain - .data$mean_period_norain,
                  dev_percent_raindays = .data$percent_raindays - .data$mean_period_percent_raindays,
                  z_percent_raindays = (.data$percent_raindays - .data$mean_period_percent_raindays)/.data$sd_period_percent_raindays)

  if (wide) {
    # Long-term columns won't be separated out into separate columns for each
    # year
    long_term_cols <- c("mean_period_total_season", "sd_period_total_season",
                        "mean_period_norain", "sd_period_norain",
                        "mean_period_raindays", "sd_period_raindays",
                        "mean_period_percent_raindays", "sd_period_percent_raindays")
    rain_summary <- wide_summary(x = rain_summary,
                                 id_col = id_column_name,
                                 long_term_cols = long_term_cols)
  }

  return(rain_summary)
}
