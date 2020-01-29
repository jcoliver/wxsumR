# Attempt to use ff package so as to preserve some RAM
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-28

rm(list = ls())

################################################################################
# Attempt to use ff package, which references data on disk instead of loading 
# into RAM. Just using ff with original data read probably won't change much,
# but perhaps it's a start. Nope. All sorts of crashy-crash.

library(weathercommand)
library(ff)

# infile <- "data/input-rain-small.csv"
infile <- "data/input-rain-medium.csv"
# infile <- "data/input-rain-large.csv"

test_data <- ff::read.csv.ffdf(file = infile)

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
# Time reporting
orig_time <- difftime(time1 = orig_end, time2 = orig_start, units = "secs")
orig_time <- round(x = orig_time, digits = 3)
message(paste0("Original implementation time: ", orig_time, " seconds"))

par_time <- difftime(time1 = par_end, time2 = par_start, units = "secs")
par_time <- round(x = par_time, digits = 3)
message(paste0("Parallel implementation time: ", par_time, " seconds"))

par_sum_time <- difftime(time1 = par_sum_end, time2 = par_sum_start, units = "secs")
par_sum_time <- round(x = par_sum_time, digits = 3)
message(paste0("Time for parLapply call: ", par_sum_time, " seconds"))
message(paste0("Additional parallel processing time (incl. split call): ", 
               round(par_time - par_sum_time, digits = 3), " seconds"))

