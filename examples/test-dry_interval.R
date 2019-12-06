# Test dry_interval function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-17

rm(list = ls())

################################################################################
# Because dry_interval is not exported, it isn't available for use outside of 
# the weathercommand package
source(file = "R/dry_interval.R")

# Test 1, season includes starting, middle, and ending dry stretches
test_num <- 1
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(0, 2, 2, 0, 0, 2, 0, 2, 0, 0, 0)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(1, 2, 3))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 2, season starts with a rain day
test_num <- 2
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(2, 2, 2, 0, 0, 2, 0, 2, 0, 0, 0)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(0, 2, 3))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 3, season ends with a rain day
test_num <- 3
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(0, 2, 2, 0, 0, 2, 2, 0, 0, 0, 2)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(1, 3, 0))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 4, season starts and ends with dry days, but only rain in between
test_num <- 4
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(1, 0, 4))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 5, season starts with dry days, but only rain afterwards
test_num <- 5
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(3, 0, 0))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 6, entire season is dry except for last day
test_num <- 6
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(10, 0, 0))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 7, season ends with dry days, but only rain beforehand
test_num <- 7
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(0, 0, 2))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))

# Test 8, entire season is dry except for first day
test_num <- 8
message(paste0("Running test ", test_num, " of dry_interval"))
rain <- c(2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

test_start <- Sys.time()
dry_test <- c(dry_interval(x = rain, period = "start"),
              dry_interval(x = rain, period = "mid"),
              dry_interval(x = rain, period = "end"))
test_end <- Sys.time()
if (all(dry_test == c(0, 0, 10))) {
  message("Test ", test_num, " PASS")
} else {
  message("Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(x = test_time, digits = 3)
message(paste0("Test ", test_num, " time: ", test_time, " minutes"))
