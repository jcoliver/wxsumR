# Attempt to parallize temperature summary
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-02-12

rm(list = ls())

################################################################################
# On 8-core desktop:
# Data    original   rbr-||  smart-||
# small      1.4       1.5      0.5
# medium    13.1      14.3      3.7
# large     60.2     138.6     42.8

library(weathercommand)
library(parallel)
library(tidyverse)

# infile <- "data/input-temperature-small.csv"
# infile <- "data/input-temperature-medium.csv"
infile <- "data/input-temperature-large.csv"

test_data <- read.csv(file = infile)

start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

########################################
# Original, serial implementation
orig_start <- Sys.time()
temperature_summary <- summarize_temperature(temperature = test_data,
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
  dplyr::group_by(hhid) %>%
  dplyr::group_split()

par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_temperature,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)

stopCluster(cl = clust)
# temperature_summary <- unsplit(value = par_summary, f = seq(length(par_summary)))
temperature_summary_rbr_par <- dplyr::bind_rows(par_summary)

# Need to re-order, first by year, then by hhid to be consistent with serial
# output
temperature_summary_rbr_par <- temperature_summary_rbr_par %>%
  arrange(season_year, hhid)

rbr_par_end <- Sys.time()

# Time reporting
rbr_par_time <- difftime(time1 = rbr_par_end, time2 = rbr_par_start, units = "secs")
rbr_par_time <- round(x = rbr_par_time, digits = 3)
message(paste0("Row-by-row || implementation time: ", rbr_par_time, " seconds"))

####################
# Parallel with list of length num_cores
num_cores <- detectCores() - 1
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
                         fun = summarize_temperature,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)

stopCluster(cl = clust)
# temperature_summary <- unsplit(value = par_summary, f = seq(length(par_summary)))
temperature_summary_smart_par <- dplyr::bind_rows(par_summary)

# Need to re-order, first by year, then by hhid to be consistent with serial
# output
temperature_summary_smart_par <- temperature_summary_smart_par %>%
  arrange(season_year, hhid)

smart_par_end <- Sys.time()

# Time reporting
smart_par_time <- difftime(time1 = smart_par_end, time2 = smart_par_start, units = "secs")
smart_par_time <- round(x = smart_par_time, digits = 3)
message(paste0("'Smart' || implementation time: ", smart_par_time, " seconds"))
