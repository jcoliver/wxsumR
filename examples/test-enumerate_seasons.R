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
df <- read.csv(file = "data/small-input.csv")
long_df <- to_long(data = df)
# Exclude NA dates
long_df <- long_df[!is.na(long_df$date), ]

# enumerate_seasons starts about here
start_month = 11
end_month = 02
day = 15

# Make sure data ordered by date
long_df <- long_df[order(long_df$date), ]

# Year of first row serves as starting point
current_season_year <- year(long_df$date[1])
current_season_start <- as.Date(paste0(current_season_year, "-", start_month, "-", day), format = "%Y-%m-%d")

# Need to see if there is a season in the starting year. If data do not start on 
# January 1, and the season includes dates that occur before the first date of 
# the date, then the starting_year should be incremented.
# For example, if data start at September 1, 1983, and the season of interest is
# March 15 - July 15, the first current_season_year will be 1984.
if (long_df$date[1] > current_season_start) {
  current_season_year <- current_season_year + 1
  current_season_start <- as.Date(paste0(current_season_year, "-", start_month, "-", day), format = "%Y-%m-%d")
}

current_season_end <- as.Date(paste0(current_season_year, "-", end_month, "-", day), format = "%Y-%m-%d")

# Need to make sure end is actually after start
if (current_season_end < current_season_start) {
  current_season_end <- current_season_end + years(x = 1)
}

# TODO: Might be a more efficient way of doing this, based on starting year and 
# ending year:
# Season will be -((minimum year - 1) - (season year))
# i.e. if oldest year is 1983, that's -((1983 - 1) - 1983), or 
# -(1982 - 1983) = 1 for 1983; Assumes that if we start with 1983 data, there 
# WILL be a season, which many not be a valid assumption. i.e. if data start 
# at September, and season is March - July, this doesn't work.

#' start by finding youngest year and oldest year
#' For months completely within the 

# Column that will ultimately hold the vector enumerating seasons
long_df$season_year <- NA
# season_year <- integer(length = nrow(long_df))
# Logical for easier processing of conditionals
in_season <- FALSE
# TODO: Flaw in logic somewhere.
# Dates are not continuous. Sampling stops on August 31 and resumes on January 1
# of subsequent year
for(i in 1:nrow(long_df)) {
  # Extract the date of current row
  current_date <- long_df$date[i]
  # Season enumeration logic
  #    If the date is at or after the current season start AND at or before
  #    the current season start, update the season_year column and ensure 
  #    in_season is true
  #    If not, and the in_season variable is set to TRUE, flip in_season to 
  #    FALSE and increment current_season and current_season_year
  
  # Debugging code; identified issue with non-existent dates, e.g. 1983-02-30
  tryCatch({
    if (current_date >= current_season_start &&
        current_date <= current_season_end) {
      # if stuff here
    }
    }, warning = function(w) {
      message("Warning in loop")
    }, error = function(e) {
      message("Error encountered in conditional")
      message(paste0("current_date = ", current_date))
      message(paste0("current_season_start = ", current_season_start))
      message(paste0("current_season_end = ", current_season_end))
    }, finally = {
      # Cleanup, if necessary
    })
  
  if (current_date >= current_season_start &&
      current_date <= current_season_end) {
    long_df$season_year[i] <- current_season_year
    # season_year[i] <- current_season_year
    in_season <- TRUE
  } else if (in_season && current_date > current_season_end) {
    # Increment the current_season_year
    current_season_year <- current_season_year + 1
    # Increment season years
    current_season_start <- current_season_start + years(x = 1)
    current_season_end <- current_season_end + years(x = 1)
    # Flip in_season to FALSE, so no more evaluations until we reach a date that 
    # lies within a season
    in_season <- FALSE
  }
}

