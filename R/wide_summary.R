#' Wide-formatted summary data
#'
#' @description \code{wide_summary} converts long-formatted summary data to
#' wide-format. Season-specific statistics are characterized by column names
#' ending in "_YYYY", where "YYYY" is the four-digit year the season
#' corresponds to. Long-term, season-independent statistics lack this suffix.
#'
#' @details The function generally works from long-formatted data, within one
#' of the \code{summarize_*} functions. When transforming to wide,
#' season-specific statistics (e.g. mean total rainfall for a season) are
#' represented in a separate column for each season, with column names appended
#' with the year of the season in "_YYYY" format. See specific
#' \code{summarize_*} functions (below) for more details about output. The
#' long-formatted data may include statistics that are not specific to a
#' particular season (e.g. the mean total seasonal rainfall across all seasons,
#' calculated by \code{\link{summarize_rainfall}}). Even though the
#' long-formatted data will represent those values in multiple rows (i.e. one
#' row for each year/site combination), the values are identical and do not
#' require separate columns in wide-formatted data. Thus each season-independent
#' statistic is represented by a single column in the resultant wide-formatted
#' data.
#'
#' @param x               summary data in pseudo-wide format
#' @param id_col          name of column with id value
#' @param year_col        name of column with year value
#' @param long_term_cols  vector of names of columns not to be transformed
#' to separate values for each year
#'
#' @return wide-formatted tibble, where summary statistics for individual years
#' are in separate columns
#'
#' @seealso \code{\link{summarize_rainfall}}, \code{\link{summarize_temperature}}
#'
#' @examples
#' \dontrun{
#' # An example, although this function should not be called on its own; rather,
#' when using any of the \code{summarize_} or \code{par_summarize_} functions,
#' pass \code{wide = FALSE}.
#' # Generate "long" format summary statistics
#' temperature_summary_long <- summarize_temperature(temperature = temperature_2yr,
#'                                                   start_month = 3,
#'                                                   end_month = 11,
#'                                                   wide = FALSE)
#' # Identify those columns that do not have year_specific values
#' long_term_cols <- c("mean_gdd", "sd_gdd")
#' # Identify the column that has the site unique id
#' id_column_name <- colnames(temperature_summary_long)[2]
#' # Convert to "wide" format
#' temperature_summary <- wxsumR:::wide_summary(x = temperature_summary_long,
#'                                                      id_col = id_column_name,
#'                                                      long_term_cols = long_term_cols)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
wide_summary <- function(x, id_col, year_col = "season_year", long_term_cols = "") {
  # Start by converting to long format
  x_long <- x %>%
    tidyr::pivot_longer(-c(id_col, year_col, long_term_cols))

  # Convert annual stats to wide format, appending year to column name
  x_wide <- x_long %>%
    tidyr::pivot_wider(id_cols = id_col,
                names_from = c(.data$name, year_col),
                values_from = .data$value,
                names_sep = "_")

  # Extract long-term statistics from original data, and restrict to unique
  # rows (an individual site _should_ have identical values across all years
  # within each column listed in long_term_cols)
  long_term_stats <- dplyr::ungroup(x) %>%
    dplyr::select(id_col, long_term_cols) %>%
    dplyr::distinct()

  # Identify shared columns to avoid messaging from dplyr::full_join
  merge_cols <- dplyr::intersect(x = colnames(x = x_wide),
                                 y = colnames(long_term_stats))

  # Merge long-formatted data with long-term stats
  merged_wide <- dplyr::full_join(x = x_wide,
                                  y = long_term_stats,
                                  by = merge_cols)

  return(merged_wide)
}
