# Compare temperature output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

library(haven)   # to read output of wxsum (STATA) output
library(dplyr)   # some data wrangling fun 
library(ggplot2) # for visualizing differences

devtools::load_all() # to make sure the package is loaded

# TODO: sample data below has no killing degree days (all temperature values in 
# the sample dataset are below the threshold of 32)

# wxsum tmp_, ini_month(11) fin_month(02) temp_data gdd_lo(8) gdd_hi(32) 
# kdd_base(32) save(temp_stats.dta)`
# start_month = 11
# end_month = 2
# growbase_low = 8
# growbase_high = 32
# (default start_day = 1)
# (default end_day = 1)

temperature <- readRDS(file = "data/temp.rds")
start_time <- Sys.time()
result_wxsumr <- temp_sum(temperature = temperature, 
                          start_month = 11, end_month = 2, 
                          growbase_low = 8, growbase_high = 32,
                          kdd_threshold = 32)
message("Completed after ", Sys.time() - start_time)
rm(start_time)
saveRDS(result_wxsumr, file = "dev-notes/temp_stats.rds")

# Read in result from stata package
result_wxsum <- read_dta(file = "dev-notes/temp_stats.dta")

# The wxsum results example includes all the original (daily) data, so remove 
# those columns for comparison sake
result_wxsum <- result_wxsum %>%
  select(-starts_with("tmp_"))

# A bunch of columns with __000[A-Z0-9]{3} pattern, 
# e.g. "__0002OK" "__00036L"

# Drop those oddities and do the data comparison
result_wxsum <- result_wxsum |>
  select(-matches("__000[A-Z0-9]{3}"))

# What's left?
wxsum_extra <- setdiff(colnames(result_wxsum), 
                       colnames(result_wxsumr))
wxsumr_extra <- setdiff(colnames(result_wxsumr),
                        colnames(result_wxsum))  
sort(wxsum_extra)
sort(wxsumr_extra)

# First off, a difference in temperature bin column naming convention
# wxsum: tempbin[0-9]{2}_YYYY
# wxsumr: tempbin_[0-9]{2}_YYYY
# (wxsum doesn't have the underscore before the bin number)

result_wxsum <- result_wxsum |>
  rename_with(.cols = matches("tempbin[0-9]{2}_[0-9]{4}"),
              ~ gsub(pattern = "tempbin",
                     replacement = "tempbin_",
                     x = .x))

wxsum_extra <- setdiff(colnames(result_wxsum), 
                       colnames(result_wxsumr))
wxsumr_extra <- setdiff(colnames(result_wxsumr),
                        colnames(result_wxsum))  
sort(wxsum_extra)
sort(wxsumr_extra)

# wxsum has mean and sd calculated for each bin:
# [1] "binmean_01" "binmean_02" "binmean_03" "binmean_04" "binsd_01"   "binsd_02"  
# [7] "binsd_03"   "binsd_04" 

# Pretty sure wxsumr shouldn't calculate values for years 1992 and 2023, as 
# they do not have complete data.

# Sidebar, for a step-through of temp_sum, declare the variables:
# start_month <- 11
# end_month <- 2
# start_day <- end_day <- 1
# growbase_low <- 10
# growbase_high <- 30
# kdd_threshold <- 32
# bins <- 4
# lr_years <- 10
# na_rm <- TRUE
# wide <- TRUE
# id_index <- 1
# date_sep <- "_"

# For now, drop those extras so we can do numeric calculations
# (and remove hhid, which is site id)
samecol_wxsum <- result_wxsum |>
  select(-all_of(wxsum_extra)) |>
  select(-hhid)
samecol_wxsumr <- result_wxsumr |>
  select(-all_of(wxsumr_extra)) |>
  select(-hhid)

# Get the columns in the same order
samecol_wxsumr <- samecol_wxsumr |>
  select(colnames(samecol_wxsum))

dif_mat <- samecol_wxsum - samecol_wxsumr
max(dif_mat, na.rm = TRUE) # 0.000684
length(which(dif_mat > 0.00005))
# Lots of small differences (1974)
col_means <- sqrt(colMeans(dif_mat^2))
sort(col_means)

# Want to do some summary visualization, so make a data frame of those diffs
col_means <- as.data.frame(col_means)
col_means$names <- rownames(col_means)
rownames(col_means) <- NULL

col_means$names <- gsub(x = col_means$names,
                        pattern = "_[0-9]{4}",
                        replacement = "")
col_means$names <- gsub(x = col_means$names,
                        pattern = "tempbin_[0-9]{2}",
                        replacement = "tempbin")
ggplot(data = col_means,
       mapping = aes(x = names, y = col_means)) +
  geom_point()
# Looking at the plot above, it's dev_gdd and gdd values that 
# the most divergent (diff 1e-04 to 2e-04). z_gdd is also a little high 
# rmse (2e-06 to 5e-06). Plotting the columns without the three gdd columns, we 
# see the remaining columns all have rmse diffs less than 8e-07.
ggplot(data = col_means |> filter(!(names %in% c("dev_gdd", "gdd", "z_gdd"))),
       mapping = aes(x = names, y = col_means)) +
  geom_point()


# Old below here
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
