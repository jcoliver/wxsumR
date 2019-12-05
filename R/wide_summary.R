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
#' @import tidyverse
wide_summary <- function(x, id_col, year_col = "year", long_term_cols = "") {
  # Start by converting to long format
  x_long <- x %>%
    pivot_longer(-c(id_cols, year_col, long_term_cols))
  
  # Convert annual stats to wide format, appending year to column name
  x_wide <- x_long %>%
    pivot_wider(id_cols = id_cols, 
                names_from = c(name, year_col), 
                values_from = value, 
                names_sep = "_")
  
  # Extract long-term statistics from original data, and restrict to unique
  # rows (an individual site _should_ have identical values across all years 
  # within each column listed in long_term_cols)
  long_term_stats <- x %>%
    select(id_cols, long_term_cols) %>%
    distinct()
  
  # Merge long-formatted data with long-term stats
  merged_wide <- merge(x = x_wide, y = long_term_stats)
  
  return(merged_wide)
}