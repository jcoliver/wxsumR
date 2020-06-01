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

#####
# BAM! Results identical 2020-06-01.
