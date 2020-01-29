# Test to see if return type (data.frame vs. tibble) matters
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-28

rm(list = ls())

################################################################################
# When returning a data frame (return_df = TRUE), RAM is 565
# When returning a tibble (return_df = FALSE), RAM is 442.5, 565, 442.6
# Looks like tibble is a little better

library(weathercommand)

# infile <- "data/input-rain-small.csv"
infile <- "data/input-rain-medium.csv"
# infile <- "data/input-rain-large.csv"

test_data <- read.csv(file = infile)

# Toggle to dictate whether a data frame or a tibble is returned
start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

########################################
message("Returning data frame")
pryr::mem_change(code = rain_summary <- summarize_rainfall(rain = test_data,
                                                           start_month = start_month,
                                                           end_month = end_month,
                                                           start_day = start_day,
                                                           end_day = end_day,
                                                           wide = FALSE,
                                                           return_df = TRUE)
                 )

message("Returning tibble")
pryr::mem_change(code = rain_summary <- summarize_rainfall(rain = test_data,
                                                           start_month = start_month,
                                                           end_month = end_month,
                                                           start_day = start_day,
                                                           end_day = end_day,
                                                           wide = FALSE,
                                                           return_df = FALSE)
)
