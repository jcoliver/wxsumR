#' A wrapper for \code{summarize_rainfall} to perform summary statistics 
#' calculations in parallel
#'
#' @param num_cores    integer indicating number of processors to use; if 
#' \code{NULL} (default), uses one fewer than the number of processors available
#' @param ...          additional values passed to \code{\link{summarize_rainfall}}
#'
#' @return tibble with rainfall summary statistics
#'
#' @seealso \code{\link{summarize_rainfall}}, \code{\link{par_summarize_temperature}}
#' @export
#' @importFrom dplyr group_by group_split bind_rows
#' @import parallel
par_summarize_rainfall <- function(num_cores = NULL, ...) {
  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1
  }
  
  clust <- parallel::makeCluster(num_cores)
  
  # Need to explicitly make weathercommand available on each node
  parallel::clusterEvalQ(clust, library(weathercommand))
  
  # Split data into num_cores data frames. To do so, need to create an
  # indicator by which to split (can work with split or dplyr::group_split)
  split_var <- sort(rep(x = 1:num_cores, length = nrow(test_data)))
  test_data$split_var <- split_var[1:nrow(test_data)]
  
  # Create a list, which is needed by parLapply
  test_list <-  test_data %>%
    dplyr::group_by(split_var) %>%
    dplyr::group_split()
  
  # Run summarize_rainfall in parallel, with arguments for that function being 
  # passed via ...
  par_summary <- parallel::parLapply(cl = clust,
                                     X = test_list,
                                     fun = summarize_rainfall,
                                     ...)
  
  parallel::stopCluster(cl = clust)

  rain_summary_smart_par <- dplyr::bind_rows(par_summary)
  
  # Re-order, first by year, then by id column to be consistent with serial
  # output
  rain_summary_smart_par <- rain_summary_smart_par %>%
    arrange(season_year, y4_hhid)
  
  smart_par_end <- Sys.time()
}