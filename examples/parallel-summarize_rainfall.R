# Attempt to parallize rain summary
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-08

rm(list = ls())

################################################################################
# Doing this with better parallelization, we see the advantage of parallel
# (finally!). On 4-core laptop:
# Data    original   rbr-||  smart-||
# small      2.8       3.2      1.6
# medium    32.3      41.4     19.5
# On 8-core desktop:
# Data    original   rbr-||  smart-||
# small      1.3       1.8      0.4
# medium     9.9      13.2      2.9
# large     47.5     151.9     16.3

library(weathercommand)
library(parallel)
library(tidyverse)

# infile <- "data/rain-small.Rds"
infile <- "data/rain-medium.Rds"
# infile <- "data/rain-large.Rds"
test_data <- readRDS(file = infile)

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

# Time reporting
orig_time <- difftime(time1 = orig_end, time2 = orig_start, units = "secs")
orig_time <- round(x = orig_time, digits = 3)
message(paste0("Original implementation time: ", orig_time, " seconds"))

########################################
# Row by row parallel, list of length nrows(test_data)
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

rbr_par_start <- Sys.time()

# Create a list, which is needed by parLapply
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
rain_summary_rbr_par <- dplyr::bind_rows(par_summary)

# Need to re-order, first by year, then by y4_hhid to be consistent with serial
# output
rain_summary_rbr_par <- rain_summary_rbr_par %>%
  arrange(season_year, y4_hhid)

rbr_par_end <- Sys.time()

# Time reporting
rbr_par_time <- difftime(time1 = rbr_par_end, time2 = rbr_par_start, units = "secs")
rbr_par_time <- round(x = rbr_par_time, digits = 3)
message(paste0("Row-by-row || implementation time: ", rbr_par_time, " seconds"))

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

# Create a list, which is needed by parLapply
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
rain_summary_smart_par <- dplyr::bind_rows(par_summary)

# Need to re-order, first by year, then by y4_hhid to be consistent with serial
# output
rain_summary_smart_par <- rain_summary_smart_par %>%
  arrange(season_year, y4_hhid)

smart_par_end <- Sys.time()

# Time reporting
smart_par_time <- difftime(time1 = smart_par_end, time2 = smart_par_start, units = "secs")
smart_par_time <- round(x = smart_par_time, digits = 3)
message(paste0("'Smart' || implementation time: ", smart_par_time, " seconds"))
