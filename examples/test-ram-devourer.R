# A serial approach to the RAM-devouring problem
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-07-10

rm(list = ls())

################################################################################
# Large data (>1M cells) will eat all RAM on older machines. There may be a way
# around this by running this row-by-row, filling in a pre-created data frame.
# No cbind! This approach works, but is painfully slow (like, it takes an hour
# for the large data set)
library(tidyverse)
library(wxsumR)

infile <- "../large-data-files/NPSY4_x1_rf1_daily.csv"
large_rain <- read_csv(infile)

rain_summary <- NULL

for (i in 1:nrow(large_rain)) {
  if (i %% 100 == 0) {
    message(paste0("At row ", i))
  }
  if (i == 1) {
    # Need to set up the output data frame, with the right number of rows
    first_row <- summarize_rainfall(rain = large_rain[i, ],
                                    start_month = 3,
                                    end_month = 11)
    rain_summary <- first_row
    rain_summary[2:nrow(large_rain), ] <- NA
    rm(first_row)
  } else {
    rain_summary[i, ] <- summarize_rainfall(rain = large_rain[i, ],
                                            start_month = 3,
                                            end_month = 11)
  }
}
