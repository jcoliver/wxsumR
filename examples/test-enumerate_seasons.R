# Test enumerate_seasons function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

rm(list = ls())

################################################################################
library(wxsumR)

df <- readRDS(file = "data/rain-small.Rds")

long_df <- wxsumR:::to_long(data = df)

# Exclude NA dates
long_df <- long_df[!is.na(long_df$date), ]

####################
# Test 1, season includes new year
test_num <- 1
message(paste0("Running test ", test_num, " of enumerate_seasons"))
start_month <- 11
end_month <- 02
day <- 15
test_start <- Sys.time()
enumerated_df <- wxsumR:::enumerate_seasons(data = long_df,
                                                    start_month = start_month,
                                                    end_month = end_month,
                                                    start_day = day)
test_end <- Sys.time()
if (all(table(enumerated_df$season_year) == c(4600, 9300, 9300, 9300, 9300, 9300))) {
  message("enumerate_seasons: Test ", test_num, " PASS")
} else {
  message("enumerate_seasons: Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(test_time, digits = 3)
message(paste0("enumerate_seasons: Test ", test_num, " time: ", test_time, " minutes"))

####################
# Test 2, season excludes new year
test_num <- 2
message(paste0("Running test ", test_num, " of enumerate_seasons"))
start_month <- 02
end_month <- 11
day <- 15
test_start <- Sys.time()
enumerated_df <- wxsumR:::enumerate_seasons(data = long_df,
                                                    start_month = start_month,
                                                    end_month = end_month,
                                                    start_day = day)
test_end <- Sys.time()
if (all(table(enumerated_df$season_year) == c(27400, 27500, 27400, 27400, 27400, 12800))) {
  message("enumerate_seasons: Test ", test_num, " PASS")
} else {
  message("enumerate_seasons: Test ", test_num, " FAIL")
}
test_time <- difftime(time1 = test_end, time2 = test_start, units = "mins")
test_time <- round(test_time, digits = 3)
message(paste0("enumerate_seasons: Test ", test_num, " time: ", test_time, " minutes"))
