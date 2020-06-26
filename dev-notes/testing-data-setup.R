# Create smaller data sets for testing purposes
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-08

rm(list = ls())

################################################################################
# RAIN
large_data <- read.csv(file = "../large-data-files/NPSY4_x1_rf1_daily.csv")

# Tiny data, 6 x 732 (two years); largely for documentation
# Saving as text to include reading in text file in vignette
write.csv(x = large_data[1:6, 1:732],
          file = "data/rain-2yr.csv",
          row.names = FALSE)

# Small data, 100 x 1462 (four years); largely for documentation
# This is what was used for comparison with Stata output
saveRDS(large_data[1:100, 1:1462],
        file = "data/rain-4yr.Rds")

# Small data, 100 x 2000
write.csv(x = large_data[1:100, 1:2000],
          file = "data/input-rain-small.csv",
          row.names = FALSE)
saveRDS(large_data[1:100, 1:2000],
        file = "data/rain-small.Rds")

# Medium wide data; 100 x 10000; 5 times more columns than small data
write.csv(x = large_data[1:100, 1:10000],
          file = "data/input-rain-medium-wider.csv",
          row.names = FALSE)

# Medium data, 1000 x 2000; 10 times more rows than small data
write.csv(x = large_data[1:1000, 1:2000],
          file = "data/input-rain-medium.csv",
          row.names = FALSE)
saveRDS(large_data[1:1000, 1:2000],
        file = "data/rain-medium.Rds")

# Large data; 1000 x 10000; 10 times more rows, 5 times more columns than small
write.csv(x = large_data[1:1000, 1:10000],
          file = "data/input-rain-large.csv",
          row.names = FALSE)
saveRDS(large_data[1:1000, 1:10000],
        file = "data/rain-large.Rds")

########################################
# TEMPERATURE
large_data <- read.csv(file = "../large-data-files/NPSY1_x1_t1_daily.csv")

# Tiny data, 6 x 732 (two years); largely for documentation
# Saving as text to include reading in text file in vignette
write.csv(x = large_data[1:6, 1:732],
          file = "data/temperature-2yr.csv",
          row.names = FALSE)

# Small data, 100 x 1462 (four years); largely for documentation
# This is what was used for comparison with Stata output
saveRDS(large_data[1:100, 1:1462],
        file = "data/temperature-4yr.Rds")

# Small data, 100 x 2000
write.csv(x = large_data[1:100, 1:2000],
          file = "data/input-temperature-small.csv",
          row.names = FALSE)
saveRDS(large_data[1:100, 1:2000],
        file = "data/temperature-small.Rds")

# Medium data, 1000 x 2000; 10 times more rows than small data
write.csv(x = large_data[1:1000, 1:2000],
          file = "data/input-temperature-medium.csv",
          row.names = FALSE)
saveRDS(large_data[1:1000, 1:2000],
        file = "data/temperature-medium.Rds")

# Large data; 1000 x 10000; 10 times more rows, 5 times more columns than small
write.csv(x = large_data[1:1000, 1:10000],
          file = "data/input-temperature-large.csv",
          row.names = FALSE)
saveRDS(large_data[1:1000, 1:10000],
        file = "data/temperature-large.Rds")
