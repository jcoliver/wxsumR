# Test enumerate_seasons function
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

################################################################################
source(file = "R/to_long.R")
source(file = "R/enumerate_seasons.R")

library(tidyverse)
df <- read.csv(file = "data/tiny-input.csv")
long_df <- to_long(data = df)

start_month = 11
end_month = 02
day = 15

# The current season stuff should come out of the for loop. Just use the first 
# row of data to establish starting values for those variables

for(i in 1:nrow(long_df)) {
  # TODO: if using lubridate, can make this next line easier
  current_year <- as.numeric(format(x = long_df$date[i], format = "%Y"))
  current_season_start <- as.Date(paste0(current_year, "-", start_month, "-", day), format = "%Y-%m-%d")
  current_season_end <- as.Date(paste0(current_year, "-", end_month, "-", day), format = "%Y-%m-%d")
  # Need to make sure end is actually after start
  if (current_season_end < current_season_start) {
    current_season_end <- as.Date(paste0((current_year + 1), "-", end_month, "-", day), format = "%Y-%m-%d")
  }
  
  if (i < 50) {
    cat("Date: ", as.character(long_df$date[i]), ", year: ", current_year, "\n", sep = "")
  }
}
