# Test summarize_temperature function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-18

rm(list = ls())

################################################################################
library(wxsumR)

# infile <- "data/temperature-small.Rds"
infile <- "data/temperature-medium.Rds"
# infile <- "data/temperature-large.Rds"
test_data <- readRDS(file = infile)

########################################
# Test 1, season includes new year
test_num <- 1
message(paste0("Running test ", test_num, " of summarize_temperature"))
start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

# Test the summarize_temperature function by performing one of the calculations
# (mean_season) on original data. Need to do some wrangling on these test data
# to perform the calculations
# Create vector of the column names we want for 1983 season data
end_date <- as.Date(paste0("1984-", end_month, "-", end_day))
current_date <- as.Date(paste0("1983-", start_month, "-", start_day))
test_col_names <- character(as.numeric(end_date - current_date))
counter <- 1
while (current_date <= end_date) {
  test_col_names[counter] <- paste0("tmp_", format(current_date, "%Y%m%d"))
  current_date <- current_date + 1
  counter <- counter + 1
}

# Calculate the row mean for this subset of data; results should correspond to
# values in the mean_season column in output of summarize_temperature
season_means <- rowMeans(x = test_data[order(test_data$hhid), test_col_names])

test_start <- Sys.time()
temperature_summary <- summarize_temperature(temperature = test_data,
                                             start_month = start_month,
                                             end_month = end_month,
                                             start_day = start_day,
                                             end_day = end_day,
                                             wide = FALSE)
test_end <- Sys.time()
if (all(season_means == temperature_summary$mean_season[temperature_summary$season_year == 1983])) {
  message("summarize_temperature: Test ", test_num, " PASS")
} else {
  message("summarize_temperature: Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("summarize_temperature: Test ", test_num, " time: ", test_time, " minutes"))

########################################
# Test 2, season excludes new year
test_num <- 2
message(paste0("Running test ", test_num, " of summarize_temperature"))
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
  test_col_names[counter] <- paste0("tmp_", format(current_date, "%Y%m%d"))
  current_date <- current_date + 1
  counter <- counter + 1
}

# Calculate the row mean for this subset of data; results should correspond to
# values in the mean_season column in output of summarize_temperature
season_means <- rowMeans(x = test_data[order(test_data$hhid), test_col_names])

test_start <- Sys.time()
temperature_summary <- summarize_temperature(temperature = test_data,
                                             start_month = start_month,
                                             end_month = end_month,
                                             start_day = start_day,
                                             end_day = end_day,
                                             wide = FALSE)
test_end <- Sys.time()
if (all(season_means == temperature_summary$mean_season[temperature_summary$season_year == 1983])) {
  message("summarize_temperature: Test ", test_num, " PASS")
} else {
  message("summarize_temperature: Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("summarize_temperature: Test ", test_num, " time: ", test_time, " minutes"))

########################################
# Test 3, season includes new year (same as test 1) but output wide format data
test_num <- 3
message(paste0("Running test ", test_num, " of summarize_temperature"))
start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

# Test the summarize_temperature function by performing one of the calculations
# (mean_season) on original data. Need to do some wrangling on these test data
# to perform the calculations
# Create vector of the column names we want for 1983 season data
end_date <- as.Date(paste0("1984-", end_month, "-", end_day))
current_date <- as.Date(paste0("1983-", start_month, "-", start_day))
test_col_names <- character(as.numeric(end_date - current_date))
counter <- 1
while (current_date <= end_date) {
  test_col_names[counter] <- paste0("tmp_", format(current_date, "%Y%m%d"))
  current_date <- current_date + 1
  counter <- counter + 1
}

# Calculate the row mean for this subset of data; results should correspond to
# values in the mean_season column in output of summarize_temperature
season_means <- rowMeans(x = test_data[order(test_data$hhid), test_col_names])

test_start <- Sys.time()
temperature_summary <- summarize_temperature(temperature = test_data,
                                             start_month = start_month,
                                             end_month = end_month,
                                             start_day = start_day,
                                             end_day = end_day,
                                             wide = TRUE)
test_end <- Sys.time()
if (all(season_means == temperature_summary$mean_season_1983)) {
  message("summarize_temperature: Test ", test_num, " PASS")
} else {
  message("summarize_temperature: Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("summarize_temperature: Test ", test_num, " time: ", test_time, " minutes"))
