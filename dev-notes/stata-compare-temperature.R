# Compare temperature output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

rm(list = ls())

################################################################################
library(tidyverse)
r_results_file <- "data/r-temperature-output.csv"
stata_results_file <- "data/stata-temp-output.csv"
id_column <- "hhid"

r_results <- read_csv(r_results_file)
stata_results <- read_csv(stata_results_file)

# R results puts out column names for bins with a space between the bin and
# the year, e.g. tempbin20_1983, while Stata output lacks the underscore, e.g.
# tempbin201983
grep(pattern = "tempbin20", x = colnames(r_results), value = TRUE)
grep(pattern = "tempbin20", x = colnames(stata_results), value = TRUE)

# For now, just rename the columns in the Stata output
tempbins <- paste0("tempbin", c(20, 40, 60, 80, 100))
for (tempbin in tempbins) {
  colnames(stata_results) <- gsub(pattern = tempbin,
                                  replacement = paste0(tempbin, "_"),
                                  x = colnames(stata_results))
}
# Checking substitution
grep(pattern = "tempbin", x = colnames(stata_results), value = TRUE)

# Confirming all columns are identical
dplyr::setdiff(colnames(r_results), colnames(stata_results))
dplyr::setdiff(colnames(stata_results), colnames(r_results))

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
rownames(result_diffs) <- unlist(r_results[, id_column])

# Where are differences "large"
big_diffs <- abs(result_diffs) > delta_cutoff
cols_with_big_diff <- colSums(big_diffs)[colSums(big_diffs) > 0]

# All the z_gdd_YYYY are returning NA for colSums
r_results$z_gdd_1986[15:19]
stata_results$z_gdd_1986[15:19]

# Are all z_gdd values NA?
r_z_gdd <- r_results[, c("z_gdd_1983", "z_gdd_1984", "z_gdd_1985", "z_gdd_1986")]
summary(r_z_gdd)
# Yes, all sd_gdd values are zero, sending a 0 to the denominator for the
# calculation of the z-score. Same result in Stata output. Do not worry about
# for now
cols_with_big_diff <- cols_with_big_diff[!is.na(cols_with_big_diff)]

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
rownames(result_rhos) <- unlist(r_results[, id_column])

big_rhos <- result_rhos > rho_cutoff
cols_with_big_rho <- colSums(big_rhos)[colSums(big_rhos) > 0]
cols_with_big_rho <- cols_with_big_rho[!is.na(cols_with_big_rho)]

########################################
# Focus on tempbin, which are different (previous versions of the R package did
# not divide the number of days by total; that has been fixed)
r_results$tempbin20_1983[1:5]
stata_results$tempbin20_1983[1:5]

# For results, are the values summing to 1? Inclusion/exclusion of values in
# the percentile bins, may be effecting the differences. i.e. inclusive or
# exclusive ranges. Since we start with counts, then divide, there is potential
# for values to be included in two bins (at least)

tempbin_1983_cols <- paste0("tempbin", seq(from = 20, to = 100, by = 20), "_1983")
# Create little data frames with just 1983 bins
r_tempbin_1983 <- r_results[, c(id_column, tempbin_1983_cols)]
# Just some isoteric syntax to get the id column to a character without
# actually referring to the column by name. For fun times about dealing with
# column names in an environment variable, look at documentation for :=
# via ?":="
r_tempbin_1983 <- r_tempbin_1983 %>%
  mutate(!!id_column := as.character(!!as.name(id_column)))

stata_tempbin_1983 <- stata_results[, c(id_column, tempbin_1983_cols)]
stata_tempbin_1983 <- stata_tempbin_1983 %>%
  mutate(!!id_column := as.character(!!as.name(id_column)))

# Ensure rows of bins are summing to 1
r_binsum_1983 <- rowSums(r_tempbin_1983[, tempbin_1983_cols])
stata_binsum_1983 <- rowSums(stata_tempbin_1983[, tempbin_1983_cols])
mean(r_binsum_1983)
mean(stata_binsum_1983)

# Pull out the first id as a character vector (more fun with environment
# variables! bang bang)
id_of_interest <- r_tempbin_1983[22, id_column] %>%
  pull(!!id_column)

# The original data!
orig_data <- read.csv(file = "data/stata-temperature.csv")

# One row of interest
one_row <- orig_data %>%
  filter(!!as.name(id_column) == id_of_interest) %>%
  mutate(!!id_column := as.character(!!as.name(id_column)))

# Get data for the one season of interest 1983, 15 March through 15 November
# Pattern
# PROBLEM with pattern. Includes all days of March & November
mar_pattern <- paste0("tmp_198303", "(",
                      paste0("(", seq(from = 15, to = 31, by = 1), ")", collapse = "|"),
                      ")")
middle_pattern <- "tmp_1983((04)|(05)|(06)|(07)|(08)|(09)|(10))[0-9]{2}"
nov_pattern <- paste0("tmp_198311", "(",
                      "(01)|(02)|(03)|(04)|(05)|(06)|(07)|(08)|(09)|(10)|(11)|(12)|(13)|(14)|(15)",
                      # paste0("(", seq(from = 1, to = 15, by = 1), ")", collapse = "|"),
                      ")")
pattern <- paste0("(", mar_pattern, ")|",
                  "(", middle_pattern, ")|",
                  "(", nov_pattern, ")")

season_col_names <- grep(pattern = pattern,
                         x = colnames(one_row),
                         value = TRUE)

one_season <- one_row[, c(id_column, season_col_names)]

temp_quantiles <- quantile(x = as.matrix(one_season[, season_col_names]),
                           probs = c(seq(from = 0, to = 1.0, by = 0.2)))

# Calculate percent of days in each bin
temp_bins <- numeric(5)
names(temp_bins) <- paste0("t", seq(from = 20, to = 100, by = 20))
temp_bins[1] <- sum(one_season[, season_col_names] < temp_quantiles[2])/length(season_col_names)
temp_bins[2] <- sum(one_season[, season_col_names] >= temp_quantiles[2] &
               one_season[, season_col_names] < temp_quantiles[3])/length(season_col_names)
temp_bins[3] <- sum(one_season[, season_col_names] >= temp_quantiles[3] &
               one_season[, season_col_names] < temp_quantiles[4])/length(season_col_names)
temp_bins[4] <- sum(one_season[, season_col_names] >= temp_quantiles[4] &
               one_season[, season_col_names] < temp_quantiles[5])/length(season_col_names)
temp_bins[5] <- sum(one_season[, season_col_names] >= temp_quantiles[5])/length(season_col_names)

# manual calculation matches R output, not Stata
temp_bins
r_results[r_results[, id_column] == id_of_interest, c(id_column, tempbin_1983_cols)]
stata_results[stata_results[, id_column] == id_of_interest, c(id_column, tempbin_1983_cols)]

# Could the Stata percentiles be based on temperatures at ALL sites, rather
# than a site-by-site basis as coded in R?

# Create quantiles based on data at all sites
temp_data_1983 <- as.matrix(orig_data[, season_col_names])

temp_quantiles_all <- quantile(x = temp_data_1983,
                               probs = c(seq(from = 0, to = 1.0, by = 0.2)))

# See how well season of interest matches those quantiles
temp_bins_all <- numeric(5)
names(temp_bins_all) <- paste0("t", seq(from = 20, to = 100, by = 20))
num_days <- length(season_col_names)
temp_bins_all[1] <- sum(one_season[, season_col_names] < temp_quantiles_all[2])/num_days
temp_bins_all[2] <- sum(one_season[, season_col_names] >= temp_quantiles_all[2] &
                      one_season[, season_col_names] < temp_quantiles_all[3])/num_days
temp_bins_all[3] <- sum(one_season[, season_col_names] >= temp_quantiles_all[3] &
                      one_season[, season_col_names] < temp_quantiles_all[4])/num_days
temp_bins_all[4] <- sum(one_season[, season_col_names] >= temp_quantiles_all[4] &
                      one_season[, season_col_names] < temp_quantiles_all[5])/num_days
temp_bins_all[5] <- sum(one_season[, season_col_names] >= temp_quantiles_all[5])/num_days

# Compare single-site quantiles to quantiles based on all sites
temp_bins
temp_bins_all

# Hmm...identical, but not too surprising as basing the percentiles on all
# sites only changes the two extreme values (0% and 100%):
temp_quantiles
temp_quantiles_all
