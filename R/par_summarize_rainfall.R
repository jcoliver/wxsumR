#' Rainfall summary statistics calculated in parallel
#'
#' @description A wrapper for \link{summarize_rainfall} to perform rainfall
#' summary statistics calculations in parallel.
#'
#' @param rain         data frame with daily rainfall data for each site
#' @param num_cores    integer indicating number of processors to use; if
#' \code{NULL} (default), uses one fewer than the number of processors available
#' @param id_index     integer column index of unique site id
#' @param ...          additional values passed to
#'                     \code{\link{summarize_rainfall}}
#'
#' @return tibble with rainfall summary statistics
#'
#' @seealso \code{\link{summarize_rainfall}}
#'
#' @examples
#' \donttest{
#' df <- readRDS(file = "data/rain-small.Rds")
#' # Season defined by 15 March through 15 November
#' rain_summary <- par_summarize_rainfall(rain = df,
#'                                        start_month = 3,
#'                                        end_month = 11)
#' # Same as example above, but restrict use to 2 processors
#' rain_summary <- par_summarize_rainfall(rain = df,
#'                                        start_month = 3,
#'                                        end_month = 11,
#'                                        num_cores = 2)
#' }
#'
#' @export
#' @import dplyr
#' @import parallel
par_summarize_rainfall <- function(rain, num_cores = NULL, id_index = 1, ...) {
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
                                     id_index = id_index,
                                     ...)

  parallel::stopCluster(cl = clust)

  rain_summary_smart_par <- dplyr::bind_rows(par_summary)

  # Need to re-order rows to be consistent with output from serial
  # implementation, but re-ordering depends on whether or not the output is in
  # wide or long format. The former will not have a season_year column, so a
  # check for presence/absence of that column affords the type of re-ordering
  # to do
  id_column_name <- colnames(rain)[id_index]
  if ("season_year" %in% colnames(rain_summary_smart_par)) {
    # Long format, re-order by season_year, then id column
    rain_summary_smart_par <- rain_summary_smart_par %>%
      arrange(season_year, !!as.name(id_column_name))
  } else {
    # Wide format, re-order only by id column
    rain_summary_smart_par <- rain_summary_smart_par %>%
      arrange(!!as.name(id_column_name))
  }

  return(rain_summary_smart_par)
}
