# Compare rainfall output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

rm(list = ls())

################################################################################
# library(haven) # To read in STATA .dta files
library(tidyverse)
r_results_file <- "data/r-rain-output.csv"
stata_results_file <- "data/stata-rain-output.csv"

r_results <- read_csv(r_results_file)
stata_results <- read_csv(stata_results_file)

# Results have different number of columns because the R results include
# dry_start_* and dry_end_* columns for four year (resulting in eight
# additional columns)
dplyr::setdiff(colnames(r_results), colnames(stata_results))

# Remove those columns, as we cannot compare them
r_results <- r_results %>%
  select(-starts_with(match = "dry_start_"), -starts_with("dry_end_"))

# Make sure set members are identical
dplyr::setdiff(colnames(stata_results), colnames(r_results))
dplyr::setdiff(colnames(r_results), colnames(stata_results))

# Are columns in same order (if > 0, then no)
sum(colnames(r_results) != colnames(stata_results))

# Even though data sets have same column names, orders of columns are different
# Rearrange columns so order is the same
stata_results <- stata_results[, colnames(r_results)]
# Are columns in same order (if > 0, then no)
sum(colnames(r_results) != colnames(stata_results))

########################################
# Comparison 1, based on absolute differences in values
delta_cutoff <- 5e-05

result_diffs <- r_results[, 2:ncol(r_results)] -
  stata_results[, 2:ncol(stata_results)]
result_diffs <- as.matrix(result_diffs)
rownames(result_diffs) <- r_results$y4_hhid

r_results$dev_raindays_1983[15:18]
stata_results$dev_raindays_1983[15:18]

# Where are differences "large"
big_diffs <- abs(result_diffs) > delta_cutoff
cols_with_big_diff <- colSums(big_diffs)[colSums(big_diffs) > 0]
# named numeric(0)

########################################
# Comparison 2, based on relative differences in values
rho_cutoff <- 1e-4 #(0.0001, or 0.01%)

# Will divide by sum of results, but to avoid dividing my zero, change all
# zeros to a very small number (for dividing)
r_denominator <- as.matrix(r_results[, 2:ncol(r_results)] +
                             stata_results[, 2:ncol(r_results)])
r_denominator[r_denominator == 0] <- 1e-16
result_rhos <- abs((r_results[, 2:ncol(r_results)] -
                  stata_results[, 2:ncol(stata_results)]) / r_denominator)
result_rhos <- as.matrix(result_rhos)
rownames(result_rhos) <- r_results$y4_hhid

big_rhos <- result_rhos > rho_cutoff
cols_with_big_rho <- colSums(big_rhos)[colSums(big_rhos) > 0]
# named numeric(0)

# Below here was before Stata calculations got fixed.

########################################
# Comparisons
# Same results, either way. Three columns come out as different:
# dev_raindays (for each year)    dev_raindays = raindays - mean_period_raindays
# mean_period_raindays            mean_period_raindays = mean(x = raindays)
#    For each site, the mean number of raindays (days with rain over some
#    threshold)
# sd_period_raindays              sd_period_raindays = sd(x = raindays)

# Manual calculation of values
# Start with mean_period_raindays
# Mean period raindays is the mean number of raindays per season, in this
# data set, that is the mean number of raindays over four years. Start by
# looking at the values in the results file that were ultimately used to
# calculate mean_period_raindays. Since they were not flagged previously, they
# should be the same in R and Stata output.
raindays_cols <- paste0("raindays_", c(1983:1986))

r_results[1:4, c("y4_hhid", raindays_cols)]
stata_results[1:4, c("y4_hhid", raindays_cols)]
# These results are identical

# But the Stata results do not match what is expected.
# Calculate the mean
stata_rainday_mean <- rowMeans(x = stata_results[, raindays_cols])
stata_rainday_mean[1:6]
stata_results$mean_period_raindays[1:6]

r_rainday_mean <- rowMeans(x = r_results[, raindays_cols])
r_rainday_mean[1:6]
r_results$mean_period_raindays[1:6]

# Look at raindays_percent to see if there are any issues there
raindays_percent_cols <- paste0("raindays_percent_", c(1983:1986))
r_rainday_percent <- rowMeans(x = r_results[, raindays_percent_cols])
r_rainday_percent[1:6]

stata_rainday_percent <- rowMeans(x = stata_results[, raindays_percent_cols])
stata_rainday_percent[1:6]
# Nope. These look fine.

# The way Stata does the calculation may be causing issues. Using wildcard
# matching to identify columns for calculations, it looks like the mean and
# standard deviation calculations for raindays is actually using values from
# columns raindays_YYYY AND raindays_percent_YYYY
stata_combined_means <- rowMeans(x = stata_results[, c(raindays_percent_cols,
                                                       raindays_cols)])
stata_combined_means[1:6]
# [1] 16.81809 16.81809 16.44157 16.31606 16.44157 16.69258
stata_results$mean_period_raindays[1:6]
# [1] 16.81809 16.81809 16.44157 16.31606 16.44157 16.69258

# BAM! That's it.

# Everything below here is destined for the rubbish bin.

# First, extract only those columns in season, Mar 15 - Nov 15 (inclusive)
input_data <- read.csv(file = "data/stata-rain.csv")

# Need to create some date sequences to make it easier to pull out relevant
# columns
season_1983 <- seq(as.Date("1983-03-15"), as.Date("1983-11-15"), by = "days")
season_1984 <- seq(as.Date("1984-03-15"), as.Date("1984-11-15"), by = "days")
season_1985 <- seq(as.Date("1985-03-15"), as.Date("1985-11-15"), by = "days")
season_1986 <- seq(as.Date("1986-03-15"), as.Date("1986-11-15"), by = "days")

cols_1983 <- paste0("rf_", format(x = season_1983, "%Y%m%d"))
cols_1984 <- paste0("rf_", format(x = season_1984, "%Y%m%d"))
cols_1985 <- paste0("rf_", format(x = season_1985, "%Y%m%d"))
cols_1986 <- paste0("rf_", format(x = season_1986, "%Y%m%d"))



format(x = "2020-04-22", )

# dev_raindays_1983 (order of magnitude difference)
r_results[1:6, c(1,13)]
stata_results[1:6, c(1,13)]
# calculating dev_raindays for a year via: raindays - mean_period_raindays
# That is, the total number of raindays minus the mean number of raindays over
# the period investigated

input_data <- read.csv(file = "data/stata-rain.csv")
# Extract only those columns of interest
# March 15 through November 15 1983
# rf19830315 through rf19831115
season_1983 <- input_data[, c(1, 75:320)]
cols <- grep(pattern = "1983", x = colnames(input_data), value = TRUE)

# Threshold for rain is 1 (the default)
rain_threshold <- 1
raindays_1983 <- rowSums(season_1983[, 2:ncol(season_1983)] >= rain_threshold)

raindays_1983 <- data.frame(y4_hhid = season_1983$y4_hhid,
                            raindays_1983 = raindays_1983)
head(raindays_1983)

