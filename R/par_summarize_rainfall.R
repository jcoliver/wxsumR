#' A wrapper for \code{summarize_rainfall} to perform summary statistics
#' calculations in parallel
#'
#' @param rain         data frame with daily rainfall data for each site
#' @param num_cores    integer indicating number of processors to use; if
#' \code{NULL} (default), uses one fewer than the number of processors available
#' @param ...          additional values passed to \code{\link{summarize_rainfall}}
#'
#' @return tibble with rainfall summary statistics
#'
#' @seealso \code{\link{summarize_rainfall}}
#' @export
#' @import dplyr
#' @import parallel
par_summarize_rainfall <- function(rain, num_cores = NULL, ...) {
  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1
  }

  clust <- parallel::makeCluster(num_cores)

  # Need to explicitly make weathercommand available on each node
  parallel::clusterEvalQ(clust, library(weathercommand))

  # Split data into num_cores data frames. To do so, need to create an
  # indicator by which to split (can work with split or dplyr::group_split)
  split_var <- sort(rep(x = 1:num_cores, length = nrow(rain)))
  rain$split_var <- split_var[1:nrow(rain)]

  # Create a list, which is needed by parLapply
  rain_list <-  rain %>%
    dplyr::group_by(split_var) %>%
    dplyr::group_split()

  # Run summarize_rainfall in parallel, with arguments for that function being
  # passed via ...
  par_summary <- parallel::parLapply(cl = clust,
                                     X = rain_list,
                                     fun = summarize_rainfall,
                                     ...)

  parallel::stopCluster(cl = clust)

  rain_summary_smart_par <- dplyr::bind_rows(par_summary)

  # Need to re-order rows to be consistent with output from serial
  # implementation, but re-ordering depends on whether or not the output is in
  # wide or long format. The former will not have a season_year column, so a
  # check for presence/absence of that column affords the type of re-ordering
  # to do
  if ("season_year" %in% colnames(rain_summary_smart_par)) {
    # Long format, re-order by season_year, then id column
    rain_summary_smart_par <- rain_summary_smart_par %>%
      arrange(season_year, y4_hhid)
  } else {
    # Wide format, re-order only by id column
    rain_summary_smart_par <- rain_summary_smart_par %>%
      arrange(y4_hhid)
  }

  return(rain_summary_smart_par)
}
