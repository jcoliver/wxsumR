# Profiling enumerate_seasons
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-12-20

rm(list = ls())

# TODO: Look at profiling section at http://adv-r.had.co.nz/memory.html#memory-profiling
################################################################################
library(weathercommand)
library(pryr)

infile <- "data/input-rain-large.csv"
orig_data <- read.csv(file = infile)

long_data <- weathercommand:::to_long(data = orig_data)

# Exclude NA dates
long_data <- long_data[!is.na(long_data$date), ]

########################################
# enumerate_seasons

start_month <- 11
end_month <- 2
mem_change(code = 
             long_data <- weathercommand:::enumerate_seasons(data = long_data,
                                                             start_month = start_month,
                                                             end_month = end_month)
)
# -102MB Before the obs_year, start_OBSY, end_OBSY variables removed at end of function
# -102MB After the obs_year, start_OBSY, end_OBSY variables removed at end of function

