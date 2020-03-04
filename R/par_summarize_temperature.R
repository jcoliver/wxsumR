#' A wrapper for \code{summarize_temperature} to perform summary statistics
#' calculations in parallel
#'
#' @param temperature  data frame with daily temperature data for each site
#' @param num_cores    integer indicating number of processors to use; if
#' \code{NULL} (default), uses one fewer than the number of processors available
#' @param id_index     integer column index of unique site id
#' @param ...          additional values passed to \code{\link{summarize_temperature}}
#'
#' @return tibble with temperature summary statistics
#'
#' @seealso \code{\link{summarize_temperature}}
#' @export
#' @import dplyr
#' @import parallel
par_summarize_temperature <- function(temperature, num_cores = NULL, id_index = 1, ...) {
  if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 1
  }

  clust <- parallel::makeCluster(num_cores)

  # Need to explicitly make weathercommand available on each node
  parallel::clusterEvalQ(clust, library(weathercommand))

  # Split data into num_cores data frames. To do so, need to create an
  # indicator by which to split (can work with split or dplyr::group_split)
  split_var <- sort(rep(x = 1:num_cores, length = nrow(temperature)))
  temperature$split_var <- split_var[1:nrow(temperature)]

  # Create a list, which is needed by parLapply
  temperature_list <-  temperature %>%
    dplyr::group_by(split_var) %>%
    dplyr::group_split()

  # Run summarize_temperature in parallel, with arguments for that function being
  # passed via ...
  par_summary <- parallel::parLapply(cl = clust,
                                     X = temperature_list,
                                     fun = summarize_temperature,
                                     id_index = id_index,
                                     ...)

  parallel::stopCluster(cl = clust)

  temperature_summary_smart_par <- dplyr::bind_rows(par_summary)

  # Need to re-order rows to be consistent with output from serial
  # implementation, but re-ordering depends on whether or not the output is in
  # wide or long format. The former will not have a season_year column, so a
  # check for presence/absence of that column affords the type of re-ordering
  # to do
  id_column_name <- colnames(temperature)[id_index]
  if ("season_year" %in% colnames(temperature_summary_smart_par)) {
    # Long format, re-order by season_year, then id column
    temperature_summary_smart_par <- temperature_summary_smart_par %>%
      arrange(season_year, !!as.name(id_column_name))
  } else {
    # Wide format, re-order only by id column
    temperature_summary_smart_par <- temperature_summary_smart_par %>%
      arrange(!!as.name(id_column_name))
  }

  return(temperature_summary_smart_par)
}
