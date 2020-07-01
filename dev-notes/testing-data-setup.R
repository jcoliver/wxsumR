# Create smaller data sets for testing purposes
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2019-11-08

rm(list = ls())

################################################################################
# RAIN
large_rain <- read.csv(file = "../large-data-files/NPSY4_x1_rf1_daily.csv")

# Tiny data, 6 x 732 (two years); largely for documentation
rain_2yr <- large_rain[1:6, 1:732]
save(rain_2yr, file = "data/rain_2yr.rda")

# Small data, 100 x 1462 (four years); largely for documentation
# This is what was used for comparison with Stata output
rain_4yr <- large_rain[1:100, 1:1462]
save(rain_4yr, file = "data/rain_4yr.rda")

# Small data, 100 x 2000
write.csv(x = large_rain[1:100, 1:2000],
          file = "data/input-rain-small.csv",
          row.names = FALSE)
saveRDS(large_rain[1:100, 1:2000],
        file = "data/rain-small.Rds")

# Medium wide data; 100 x 10000; 5 times more columns than small data
write.csv(x = large_rain[1:100, 1:10000],
          file = "data/input-rain-medium-wider.csv",
          row.names = FALSE)

# Medium data, 1000 x 2000; 10 times more rows than small data
write.csv(x = large_rain[1:1000, 1:2000],
          file = "data/input-rain-medium.csv",
          row.names = FALSE)
saveRDS(large_rain[1:1000, 1:2000],
        file = "data/rain-medium.Rds")

# Large data; 1000 x 10000; 10 times more rows, 5 times more columns than small
write.csv(x = large_rain[1:1000, 1:10000],
          file = "data/input-rain-large.csv",
          row.names = FALSE)
saveRDS(large_rain[1:1000, 1:10000],
        file = "data/rain-large.Rds")

########################################
# TEMPERATURE
large_temperature <- read.csv(file = "../large-data-files/NPSY1_x1_t1_daily.csv")

# Tiny data, 6 x 732 (two years); largely for documentation
temperature_2yr <-large_temperature[1:6, 1:732]
save(temperature_2yr, file = "data/temperature_2yr.rda")

# Small data, 100 x 1462 (four years); largely for documentation
# This is what was used for comparison with Stata output
temperature_4yr <- large_temperature[1:100, 1:1462]
save(temperature_4yr, file = "data/temperature_4yr.rda")

# Small data, 100 x 2000
write.csv(x = large_temperature[1:100, 1:2000],
          file = "data/input-temperature-small.csv",
          row.names = FALSE)
saveRDS(large_temperature[1:100, 1:2000],
        file = "data/temperature-small.Rds")

# Medium data, 1000 x 2000; 10 times more rows than small data
write.csv(x = large_temperature[1:1000, 1:2000],
          file = "data/input-temperature-medium.csv",
          row.names = FALSE)
saveRDS(large_temperature[1:1000, 1:2000],
        file = "data/temperature-medium.Rds")

# Large data; 1000 x 10000; 10 times more rows, 5 times more columns than small
write.csv(x = large_temperature[1:1000, 1:10000],
          file = "data/input-temperature-large.csv",
          row.names = FALSE)
saveRDS(large_temperature[1:1000, 1:10000],
        file = "data/temperature-large.Rds")
