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
delta_cutoff <- 1e-05

r_results <- read_csv(r_results_file)
stata_results <- read_csv(stata_results_file)

# Results have different number of columns because the R results include
# dry_start_* and dry_end_* columns for four year (resulting in eight
# additional columns)
dplyr::setdiff(colnames(r_results), colnames(stata_results))

# Remove those columns, as we cannot compare them
r_results <- r_results %>%
  select(-starts_with(match = "dry_start_"), -starts_with("dry_end_"))
dplyr::setdiff(colnames(r_results), colnames(stata_results))

# There is a good chance we have differences due to rounding, so create a
# matrix of differences and see which columns have "big" deltas
result_diffs <- r_results[, 2:ncol(r_results)] -
  stata_results[, 2:ncol(stata_results)]
rownames(result_diffs) <- r_results$y4_hhid

# This comparison isn't working; returning TRUE when it should not
big_diffs <- result_diffs > delta_cutoff
cols_with_big_diff <- colSums(big_diffs)

# Matrix conversion isn't working
# See which columns have "big" delta
result_diffs_mat <- as.matrix(result_diffs)
big_diffs_mat <- result_diffs_mat > delta_cutoff
cols_with_big_diff <- colSums(big_diffs_mat)
# Idiosyncratic discrepancies by year. 1983 mean_season was identical between
# R and stata results, but for 1985, mean_season was greater than delta_cutoff
# all all 100 sites
r_results$mean_season_1985[1:10]
stata_results$mean_season_1985[1:10]


diff_means <- colMeans(x = result_diffs)
big_diffs <- diff_means[diff_means > delta_cutoff]
values_to_check <- names(big_diffs)
values_to_check <- gsub(pattern = "198[3456]", replacement = "YYYY",
                        x = values_to_check)
values_to_check <- unique(values_to_check)
# [1] "sd_season_YYYY"               "total_season_YYYY"
# [3] "norain_YYYY"                  "raindays_YYYY"
# [5] "dry_YYYY"                     "mean_season_YYYY"
# [7] "z_total_season_YYYY"          "median_season_YYYY"
# [9] "dev_total_season_YYYY"        "dev_norain_YYYY"
# [11] "mean_period_total_season"     "sd_period_total_season"
# [13] "mean_period_norain"           "sd_period_norain"
# [15] "mean_period_raindays"         "sd_period_raindays"
# [17] "mean_period_raindays_percent" "sd_period_raindays_percent"

# Read in data and do manual calculation
