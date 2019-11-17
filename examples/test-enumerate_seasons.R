# Test enumerate_seasons function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

rm(list = ls())

################################################################################
source(file = "R/to_long.R")
source(file = "R/enumerate_seasons.R")

library(tidyverse)
library(lubridate)
df <- read.csv(file = "data/input-small.csv")
long_df <- to_long(data = df)
# Exclude NA dates
long_df <- long_df[!is.na(long_df$date), ]

# Test 1 for season that includes new year
start_month <- 11
end_month <- 02
day <- 15
enumerated_df <- enumerate_seasons(data = long_df,
                                   start_month = start_month,
                                   end_month = end_month,
                                   day = day)
if (all(table(enumerated_df$season_year)) == 9300) {
  message("Test 1 PASS")
} else {
  message("Test 1 FAIL")
}

# Test 1 for season that includes new year
start_month <- 02
end_month <- 11
day <- 15
enumerated_df <- enumerate_seasons(data = long_df,
                                   start_month = start_month,
                                   end_month = end_month,
                                   day = day)
if (all(table(enumerated_df$season_year) == c(27400, 27500, 27400, 27400, 27400, 12800))) {
  message("Test 1 PASS")
} else {
  message("Test 1 FAIL")
}
