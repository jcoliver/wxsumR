#' Wide-formatted summary data
#' 
#' @param x               summary data in pseudo-wide format
#' @param id_col          name of column with id value
#' @param year_col        name of column with year value
#' @param long_term_cols  vector of names of columns not to be transformed 
#' to separate values for each year
#' 
#' @return wide-formatted data, where summary statistics for individual years 
#' are in separate columns
#' 
#' @import tidyr
#' @import dplyr
wide_summary <- function(x, id_col, year_col = "season_year", long_term_cols = "") {
  # Start by converting to long format
  x_long <- x %>%
    tidyr::pivot_longer(-c(id_col, year_col, long_term_cols))
  
  # Convert annual stats to wide format, appending year to column name
  x_wide <- x_long %>%
    tidyr::pivot_wider(id_cols = id_col, 
                names_from = c(name, year_col), 
                values_from = value, 
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