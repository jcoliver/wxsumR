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

# Differences above this warrant investigation
delta_cutoff <- 5e-05

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
dplyr::setdiff(colnames(r_results), colnames(stata_results))
# Are columns in same order (if > 0, then no)
sum(colnames(r_results) != colnames(stata_results))

# Even though data sets have same column names, orders of columns are different
# Rearrange columns so order is the same
stata_results <- stata_results[, colnames(r_results)]
# Are columns in same order (if > 0, then no)
sum(colnames(r_results) != colnames(stata_results))

# There is a good chance we have differences due to rounding, so create a
# matrix of differences and see which columns have "big" deltas
result_diffs <- r_results[, 2:ncol(r_results)] - stata_results[, 2:ncol(stata_results)]
result_diffs <- as.matrix(result_diffs)
rownames(result_diffs) <- r_results$y4_hhid

stata_results$dev_raindays_1983[15:18]

# Where are differences "large"
big_diffs <- abs(result_diffs) > delta_cutoff
cols_with_big_diff <- colSums(big_diffs)[colSums(big_diffs) > 0]
# dev_raindays_1983    dev_raindays_1984    dev_raindays_1985
# 100                  100                  100
# dev_raindays_1986 mean_period_raindays   sd_period_raindays
# 100                  100                  100

# Comparing percentages, not differences
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
# dev_raindays_1983    dev_raindays_1984    dev_raindays_1985
#               100                  100                  100
# dev_raindays_1986 mean_period_raindays   sd_period_raindays
#               100                  100                  100

# Same results, either way. Three differences:
# dev_raindays (for each year)
# mean_period_raindays
# sd_period_raindays

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
