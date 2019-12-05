# Development code for producing wide-formatted output
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-12-05

rm(list = ls())

################################################################################
library(tidyverse)

# General format of the output of summarize_rainfall():
outdata <- data.frame(hhid = c(1, 2, 3, 1, 2, 3),
                      year = c(1983, 1983, 1983, 1984, 1984, 1984),
                      mean_period_norain = c(10, 20, 30, 10, 20, 30),
                      mean_period_raindays = c(11, 22, 33, 11, 22, 33), 
                      mean_season = c(311, 322, 333, 411, 422, 433),
                      median_season = c(310, 320, 330, 410, 420, 430))

#   hhid year mean_period_norain mean_season median_season
# 1    1 1983                 10         311           310
# 2    2 1983                 20         322           320
# 3    3 1983                 30         333           330
# 4    1 1984                 10         411           410
# 5    2 1984                 20         422           420
# 6    3 1984                 30         433           430

# Maybe we want a fully long version...
outdata_long <- outdata %>%
  pivot_longer(-c(hhid, year, mean_period_norain, mean_period_raindays))
#     hhid  year mean_period_norain name          value
#  1     1  1983                 10 mean_season     311
#  2     1  1983                 10 median_season   310
#  3     2  1983                 20 mean_season     322
#  4     2  1983                 20 median_season   320
#  5     3  1983                 30 mean_season     333
#  6     3  1983                 30 median_season   330
#  7     1  1984                 10 mean_season     411
#  8     1  1984                 10 median_season   410
#  9     2  1984                 20 mean_season     422
# 10     2  1984                 20 median_season   420
# 11     3  1984                 30 mean_season     433
# 12     3  1984                 30 median_season   430
# ...

# Want:
# hhid  mean_period_norain mean_season_1983 median_season_1983 mean_season_1984 median_season_1984
# 1           10                 311                310              411              410
# 2           20                 322                320              422              420
# 3           30                 333                330              433              430

# This is close; year-specific columns get created, but mean_period_norain is 
# not included in output
wide_test <- outdata_long %>%
  pivot_wider(id_cols = c(hhid), 
              names_from = c(name, year), 
              values_from = value, 
              names_sep = "_")

# Could add those values back with a merge...
long_term_stats <- outdata %>%
  select(hhid, mean_period_norain, mean_period_raindays) %>%
  distinct(hhid, .keep_all = TRUE)

merged_wide <- merge(x = wide_test, y = long_term_stats)


####
# Implementation with abstracted column names
x <- outdata
id_cols <- "hhid"
year_col <- "year"
long_term_cols <- c("mean_period_norain", "mean_period_raindays")

# Start by converting to long format
x_long <- x %>%
  pivot_longer(-c(id_cols, year_col, long_term_cols))

# Convert annual stats to wide format, appending year to column name
x_wide <- x_long %>%
  pivot_wider(id_cols = id_cols, 
              names_from = c(name, year_col), 
              values_from = value, 
              names_sep = "_")

# Extract long-term statistics from original data, and restrict to unique
# rows (an individual site _should_ have identical values across all years 
# within each column listed in long_term_cols)
long_term_stats <- x %>%
  select(id_cols, long_term_cols) %>%
  distinct()

# Merge long-formatted data with long-term stats
merged_wide <- merge(x = x_wide, y = long_term_stats)


