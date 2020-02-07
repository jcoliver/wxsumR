# Attempt to parallize rain summary
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-08

rm(list = ls())

################################################################################
# Remember https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
# With small data set, parallel takes longer (0.8 vs. 2.4 seconds).
# With medium data set, parallel still takes longer (10.7 vs. 22.8 seconds); a
# substantial part of this is setting up the cluster and breaking the data into
# a list object (9.9 seconds), but even just the parallel execution of
# summarize_rainfall takes 12.8 seconds. Consider a dplyr analog to split.
# With large data set, the parallel approach takes longer (62 vs. 203 seconds);
# even just the parLapply execution takes a substantial amount of time (153)
# seconds.

# Doing this with better parallelization, we see the advantage of parallel
# (finally!). On 4-core laptop:
# Data    original   rbr-||  smart-||
# small      2.8       3.2      1.6
# medium    32.3      41.4     19.5

# There is still a MASSIVE memory leak. After running this with the large data
# and clearing the environment with rm(list = ls()), the rsession was still
# holding onto 3.4 GB of RAM. A call to gc() freed up a little bit (0.4 GB), but
# still not enough.

# UPDATE: more likely memory fragmentation than an actual leak. Little to be
# done about this. Consider smarter parallelization and using data.table instead
# of dplyr

library(weathercommand)
library(parallel)
library(tidyverse)

# infile <- "data/input-rain-small.csv"
infile <- "data/input-rain-medium.csv"
# infile <- "data/input-rain-large.csv"

test_data <- read.csv(file = infile)

start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

########################################
# Original, serial implementation
orig_start <- Sys.time()
rain_summary <- summarize_rainfall(rain = test_data,
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
orig_end <- Sys.time()

########################################
# Cluster approach for parallel
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

####################
# Row by row parallel, list of length nrows(test_data)
rbr_par_start <- Sys.time()
# I don't think the split/unsplit works quite right
# test_list <- split(x = test_data, f = seq(nrow(test_data)))
test_list <- test_data %>%
  dplyr::group_by(y4_hhid) %>%
  dplyr::group_split()

par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)

stopCluster(cl = clust)
# rain_summary <- unsplit(value = par_summary, f = seq(length(par_summary)))
rain_summary <- dplyr::bind_rows(par_summary)

rbr_par_end <- Sys.time()

####################
# Parallel with list of length num_cores
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

smart_par_start <- Sys.time()

# Attempt with better parallelization. Instead of spliting into single-row
# data frames, split into num_cores data frames. Need to start by creating an
# indicator by which to split (can work with split or dplyr::group_split)
split_var <- sort(rep(x = 1:num_cores, length = nrow(test_data)))
test_data$split_var <- split_var[1:nrow(test_data)]
# test_list <- split(x = test_data, f = test_data$split_var, drop = TRUE)
test_list <-  test_data %>%
  dplyr::group_by(split_var) %>%
  dplyr::group_split()

par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)

stopCluster(cl = clust)
# rain_summary <- unsplit(value = par_summary, f = seq(length(par_summary)))
rain_summary <- dplyr::bind_rows(par_summary)

smart_par_end <- Sys.time()

####################
# Previous implementation
# Have to make this a list of one-row data frames before sending to lapply
# test_list <- split(x = test_data, f = seq(nrow(test_data)), drop = TRUE)
# TODO: Look at dplyr::group_split, too
# test_list <- test_data %>%
#   dplyr::group_by(y4_hhid) %>%
#   dplyr::group_split()
#
# par_sum_start <- Sys.time()
# par_summary <- parLapply(cl = clust,
#                          X = test_list,
#                          fun = summarize_rainfall,
#                          start_month = start_month,
#                          end_month = end_month,
#                          start_day = start_day,
#                          end_day = end_day,
#                          wide = FALSE)
# par_sum_end <- Sys.time()
# stopCluster(cl = clust)
# rain_summary <- dplyr::bind_rows(par_summary)
# par_end <- Sys.time()

########################################
# Time reporting
orig_time <- difftime(time1 = orig_end, time2 = orig_start, units = "secs")
orig_time <- round(x = orig_time, digits = 3)
message(paste0("Original implementation time: ", orig_time, " seconds"))

rbr_par_time <- difftime(time1 = rbr_par_end, time2 = rbr_par_start, units = "secs")
rbr_par_time <- round(x = rbr_par_time, digits = 3)
message(paste0("Row-by-row || implementation time: ", rbr_par_time, " seconds"))

smart_par_time <- difftime(time1 = smart_par_end, time2 = smart_par_start, units = "secs")
smart_par_time <- round(x = smart_par_time, digits = 3)
message(paste0("'Smart' || implementation time: ", smart_par_time, " seconds"))

# par_sum_time <- difftime(time1 = par_sum_end, time2 = par_sum_start, units = "secs")
# par_sum_time <- round(x = par_sum_time, digits = 3)
# message(paste0("Time for parLapply call: ", par_sum_time, " seconds"))
# message(paste0("Additional parallel processing time (incl. split call): ",
#                round(par_time - par_sum_time, digits = 3), " seconds"))

