#' Temperature summary statistics
#'
#' @description Uses daily site data to calculate summary temperature statistics
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
#' @param temperature   data frame with daily temperature data for each site
#' @param start_month   numeric starting month defining season (inclusive)
#' @param end_month     numeric ending month defining season (inclusive)
#' @param start_day     numeric day of starting month defining season
#' (inclusive)
#' @param end_day       numeric day of ending month defining season (inclusive);
#' defaults to \code{start_day}
#' @param growbase_low  numeric lower bound for calculating growing degree days
#' (inclusive)
#' @param growbase_high numeric upper bound for calculating growing degree days
#' (inclusive)
#' @param na.rm         logical passed to summary statistic functions indicating
#' treatment of \code{NA} values
#' @param wide         logical indicating whether or not to output as wide-
#' formatted data
#' @param id_index     integer column index of unique site id
#'
#' @return tibble with temperature summary statistics
#' If \code{wide = FALSE}, returns values for each year for each site:
#' \describe{
#'   \item{mean_season}{Mean temperature for the season}
#'   \item{median_season}{Median temperature for the season}
#'   \item{sd_season}{Standard deviation of temperature for the season}
#'   \item{skew_season}{Skew of temperatures for the season, where skew is
#'   defined by (mean - median)/sd}
#'   \item{max_season}{Maximum temperature over the season}
#'   \item{gdd}{Number growing degree days, defined as days with recorded
#'   temperature between \code{growbase_low} and \code{growbase_high},
#'   inclusive}
#'   \item{tempbin20}{Number of days with temperature in the first quintile
#'   (0-20th percentile)}
#'   \item{tempbin40}{Number of days with temperature in the second quintile
#'   (20-40th percentile)}
#'   \item{tempbin60}{Number of days with temperature in the third quintile
#'   (40-60th percentile)}
#'   \item{tempbin80}{Number of days with temperature in the fourth quintile
#'   (60-80th percentile)}
#'   \item{tempbin100}{Number of days with temperature in the fifth quintile
#'   (80-100th percentile)}
#'   \item{mean_gdd}{Mean growing degree days across all seasons}
#'   \item{sd_gdd}{Standard deviation of growing degree days across all seasons}
#'   \item{dev_gdd}{Seasonal deviation from the mean number of growing degree
#'   days across all seasons}
#'   \item{z_gdd}{Difference between the number of growing degree days in a
#'   season and the mean number of growing degree days across all seasons,
#'   divided by \code{sd_gdd}}
#'
#' If \code{wide = TRUE}, all columns except \code{mean_gdd} and \code{"sd_gdd}
#' are replaced with one column for each year. For example, if the data include
#' daily measurements from 1997 to 2002, there will be no \code{mean_season}
#' column in the output, but will instead have columns \code{mean_season_1997},
#' \code{mean_season_1998}...\code{mean_season_2002}.
#'
#' @seealso \code{\link{summarize_rainfall}}, \code{\link{par_summarize_temperature}}
#'
#' #' @examples
#' \donttest{
#' df <- readRDS(file = "data/temperature-small.Rds")
#' # Season defined by 15 March through 15 November
#' temperature_summary <- summarize_temperature(temperature = df,
#'                                              start_month = 3,
#'                                              end_month = 11)
#'
#' # As example above, but output in "long" format
#' temperature_summary <- summarize_temperature(temperature = df,
#'                                              start_month = 3,
#'                                              end_month = 11,
#'                                              wide = FALSE)
#'
#' # Season defined by 30 November through 15 March
#' temperature_summary <- summarize_temperature(temperature = df,
#'                                              start_month = 11,
#'                                              end_month = 3,
#'                                              start_day = 30,
#'                                              end_day = 15)
#' }
#'
#' @export
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom stats median na.omit sd
#' @importFrom utils read.csv
summarize_temperature <- function(temperature, start_month, end_month,
                                  start_day = 15, end_day = start_day,
                                  growbase_low = 10, growbase_high = 30,
                                  na.rm = TRUE, wide = TRUE, id_index = 1) {

  # Extract name of column with site id
  id_column_name <- colnames(temperature)[id_index]

  # Use to_long to convert to long format and parse column names into dates
  temperature_long <- to_long(data = temperature, keep_cols = id_index)

  # Exclude NA dates
  temperature_long <- temperature_long %>%
    tidyr::drop_na(date)

  # Enumerate seasons
  temperature_long <- enumerate_seasons(data = temperature_long,
                                 start_month = start_month,
                                 end_month = end_month,
                                 start_day = start_day,
                                 end_day = end_day)

  # Start with calculating basic statistics, including the growing degree days
  # for the season (Here defined as number of days in season that were within
  # the range defined by (growbase_low, growbase_high))
  temperature_summary <- temperature_long %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_season = mean(x = value, na.rm = na.rm),
                     median_season = median(x = value, na.rm = na.rm),
                     sd_season = sd(x = value, na.rm = na.rm),
                     skew_season = (mean(x = value, na.rm = na.rm) - median(x = value, na.rm = na.rm))/sd(x = value, na.rm = na.rm),
                     max_season = ifelse(test = !all(is.na(value)),
                                         yes = max(value, na.rm = na.rm),
                                         no = NA), # rows of all NA return -Inf by max()
                     gdd = sum(value >= growbase_low & value <= growbase_high, na.rm = na.rm),
                     # tempbin20 = sum(value < quantile(x = value, probs = 0.2, na.rm = na.rm), na.rm = na.rm),
                     # tempbin40 = sum(value >= quantile(x = value, probs = 0.2, na.rm = na.rm) &
                     #                   value < quantile(x = value, probs = 0.4, na.rm = na.rm), na.rm = na.rm),
                     # tempbin60 = sum(value >= quantile(x = value, probs = 0.4, na.rm = na.rm) &
                     #                   value < quantile(x = value, probs = 0.6, na.rm = na.rm), na.rm = na.rm),
                     # tempbin80 = sum(value >= quantile(x = value, probs = 0.6, na.rm = na.rm) &
                     #                   value < quantile(x = value, probs = 0.8, na.rm = na.rm), na.rm = na.rm),
                     # tempbin100 = sum(value >= quantile(x = value, probs = 0.8, na.rm = na.rm), na.rm = na.rm))
                     tempbin20 = sum(value < quantile(x = value, probs = 0.2, na.rm = na.rm), na.rm = na.rm)/n(),
                     tempbin40 = sum(value >= quantile(x = value, probs = 0.2, na.rm = na.rm) &
                                       value < quantile(x = value, probs = 0.4, na.rm = na.rm), na.rm = na.rm)/n(),
                     tempbin60 = sum(value >= quantile(x = value, probs = 0.4, na.rm = na.rm) &
                                       value < quantile(x = value, probs = 0.6, na.rm = na.rm), na.rm = na.rm)/n(),
                     tempbin80 = sum(value >= quantile(x = value, probs = 0.6, na.rm = na.rm) &
                                       value < quantile(x = value, probs = 0.8, na.rm = na.rm), na.rm = na.rm)/n(),
                     tempbin100 = sum(value >= quantile(x = value, probs = 0.8, na.rm = na.rm), na.rm = na.rm)/n())

  # Calculate seasonal average and standard deviation for gdd
  temperature_summary <- dplyr::ungroup(temperature_summary) %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(mean_gdd = mean(gdd, na.rm = na.rm),
                  sd_gdd = sd(gdd, na.rm = na.rm))

  # Finally, calculate deviations from mean for each season, measured as
  # difference from the mean and as z-score
  temperature_summary <- dplyr::ungroup(temperature_summary) %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::mutate(dev_gdd = gdd - mean_gdd,
                  z_gdd = (gdd - mean_gdd)/sd_gdd)

  if (wide) {
    # Long-term columns won't be separated out into separate columns for each
    # year
    long_term_cols <- c("mean_gdd", "sd_gdd")
    temperature_summary <- wide_summary(x = temperature_summary,
                                 id_col = id_column_name,
                                 long_term_cols = long_term_cols)
  }

  return(temperature_summary)
}
