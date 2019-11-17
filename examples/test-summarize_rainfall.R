# Test summarize_rainfall function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-17

rm(list = ls())

################################################################################
source(file = "R/to_long.R")
source(file = "R/enumerate_seasons.R")
source(file = "R/summarize_railfall.R")

library(tidyverse)
library(lubridate)

infile <- "data/input-small.csv"
outfile <- NULL

# Test 1, season includes new year
test_num <- 1
message(paste0("Running test ", test_num, " of summarize_rainfall"))
start_month <- 11
end_month <- 02
day <- 15

test_start <- Sys.time()
rain_summary <- summarize_rainfall(inputfile = infile,
                   start_month = start_month,
                   end_month = end_month,
                   day = day,
                   outputfile = outfile)
test_end <- Sys.time()
if (round(x = mean(rain_summary$sd_season), digits = 3) == 6.816) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 2, season excludes new year
test_num <- 2
message(paste0("Running test ", test_num, " of enumerate_seasons"))
start_month <- 02
end_month <- 11
day <- 15
test_start <- Sys.time()
rain_summary <- summarize_rainfall(inputfile = infile,
                                   start_month = start_month,
                                   end_month = end_month,
                                   day = day,
                                   outputfile = outfile)
test_end <- Sys.time()
if (round(x = mean(rain_summary$sd_season), digits = 3) == 8.220) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))
