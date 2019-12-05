# Test summarize_rainfall function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-17

rm(list = ls())

################################################################################
source(file = "R/to_long.R")
source(file = "R/enumerate_seasons.R")
source(file = "R/summarize_rainfall.R")

library(tidyverse)
library(lubridate)

infile <- "data/input-rain-small.csv"
outfile <- NULL

test_data <- read.csv(file = infile)

# Test 1, season includes new year
test_num <- 1
message(paste0("Running test ", test_num, " of summarize_rainfall"))
start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

# Test the summarize_rainfall function by performing one of the calculations
# (mean_season) on original data. Need to do some wrangling on these test data
# to perform the calculations
# Create vector of the column names we want for 1983 season data
end_date <- as.Date(paste0("1984-", end_month, "-", end_day))
current_date <- as.Date(paste0("1983-", start_month, "-", start_day))
test_col_names <- character(as.numeric(end_date - current_date))
counter <- 1
while (current_date <= end_date) {
  test_col_names[counter] <- paste0("rf_", format(current_date, "%Y%m%d"))
  current_date <- current_date + 1
  counter <- counter + 1
}

# Calculate the row mean for this subset of data; results should correspond to
# values in the mean_season column in output of summarize_temperature
season_means <- rowMeans(x = test_data[order(test_data$y4_hhid), test_col_names])

test_start <- Sys.time()
rain_summary <- summarize_rainfall(inputfile = infile,
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
test_end <- Sys.time()
if (all(season_means == rain_summary$mean_season[rain_summary$season_year == 1983])) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 2, season excludes new year
test_num <- 2
message(paste0("Running test ", test_num, " of summarize_rainfall"))
start_month <- 02
end_month <- 11
start_day <- 15
end_day <- 25

# Create vector of the column names we want for 1983 season data
end_date <- as.Date(paste0("1983-", end_month, "-", end_day))
current_date <- as.Date(paste0("1983-", start_month, "-", start_day))
test_col_names <- character(as.numeric(end_date - current_date))
counter <- 1
while (current_date <= end_date) {
  test_col_names[counter] <- paste0("rf_", format(current_date, "%Y%m%d"))
  current_date <- current_date + 1
  counter <- counter + 1
}

# Calculate the row mean for this subset of data; results should correspond to
# values in the mean_season column in output of summarize_temperature
season_means <- rowMeans(x = test_data[order(test_data$y4_hhid), test_col_names])

test_start <- Sys.time()
rain_summary <- summarize_rainfall(inputfile = infile,
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
test_end <- Sys.time()
if (all(season_means == rain_summary$mean_season[rain_summary$season_year == 1983])) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))
