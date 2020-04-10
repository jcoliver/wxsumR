# Create R output for comparison to STATA output for rainfall
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

rm(list = ls())

################################################################################
library(weathercommand)
library(tidyverse)

# STOP: this data file has id values that have been rounded, creating many
# duplicates
input_file <- "../large-data-files/essy2_x3_rf2_daily.csv"

# Season is March 15 through November 15
start_month <- 3
start_day <- 15
end_month <- 11
end_day <- 15

rain_summary <- par_summarize_rainfall(rain = test_data,
                                       start_month = start_month,
                                       end_month = end_month,
                                       start_day = start_day,
                                       end_day = end_day,
                                       wide = TRUE)

write.csv(x = rain_summary,
          file = "data/essy2_rf_weathercommand.csv",
          row.names = FALSE)
