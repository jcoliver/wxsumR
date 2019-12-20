# Test of to_long function on medium data set, benchmarking different approaches
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-04

rm(list = ls())
################################################################################
library(weathercommand)

df <- read.csv(file = "data/input-rain-medium.csv")

# Test with original implementation in creating new column
original <- Sys.time()
long_df <- weathercommand:::to_long(data = df, 
                                    tidy_date = FALSE, 
                                    tidy_paste = FALSE)
original_time <- Sys.time() - original
rm(long_df, original) # no effect on RAM

# Test with full tidy in creating new column
full_tidy <- Sys.time()
long_df <- weathercommand:::to_long(data = df, 
                                    tidy_date = TRUE, 
                                    tidy_paste = TRUE)
full_tidy_time <- Sys.time() - full_tidy
rm(long_df, full_tidy)

# Test with tidy paste only in creating new column
tidy_paste <- Sys.time()
long_df <- weathercommand:::to_long(data = df, 
                                    tidy_date = FALSE, 
                                    tidy_paste = TRUE)
tidy_paste_time <- Sys.time() - tidy_paste
rm(long_df, tidy_paste)

# Test with tidy date only in creating new column
tidy_date <- Sys.time()
long_df <- weathercommand:::to_long(data = df, 
                                    tidy_date = TRUE, 
                                    tidy_paste = FALSE)
tidy_date_time <- Sys.time() - tidy_date
rm(long_df, tidy_date)

message(paste0("Original: ", original_time))
message(paste0("Full tidy: ", full_tidy_time))
message(paste0("Tidy paste: ", tidy_paste_time))
message(paste0("Tidy date: ", tidy_date_time))
