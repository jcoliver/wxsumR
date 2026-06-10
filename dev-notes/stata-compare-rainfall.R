# Compare rainfall output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

library(haven)  # to read output of wxsum (STATA) output
library(dplyr)  # some data wrangling fun 

# How example output was created in STATA:
# I ran ` wxsum rf_, ini_month(05) fin_month(10) ini_day(15) fin_day(15) 
# rain_data save(rainfall_stats.dta)` to generate the attached rainfall data.

# By default it outputs wide data, so no need to specify wide
devtools::load_all() # to make sure the package is loaded
# start_month = 5
# end_month = 10
# start_day = 15
# end_day = 15

rain <- readRDS(file = "data/rain.rds")
# rain <- read_dta(file = "dev-notes/rain.dta")
start_time <- Sys.time()
result_wxsumr <- rain_sum(rain = rain, start_month = 5, end_month = 10, 
                   start_day = 15, end_day = 15)
message("Completed after ", Sys.time() - start_time)
rm(start_time)
saveRDS(result_wxsumr, file = "dev-notes/rainrall_stats.rds")

# Read in result from stata package
result_wxsum <- read_dta(file = "dev-notes/rainfall_stats.dta")

# The example includes all the original (daily) data, so remove those columns 
# for comparison sake
result_wxsum <- result_wxsum %>%
  select(-starts_with("rf_"))

# A difference in column number (wxsumr:497, wxsum:740)
wxsum_extra <- setdiff(colnames(result_wxsum), 
                       colnames(result_wxsumr))
wxsumr_extra <- setdiff(colnames(result_wxsumr),
                        colnames(result_wxsum))  
sort(wxsum_extra)
sort(wxsumr_extra)

# As of 2026-06-09, only wxsum includes extra columns. A bunch (168) of columns 
# with __000[d][A-Z0-9][A-Z] pattern, e.g. "__0002OK" "__00036L"

# Drop those oddities and do the data comparison
result_wxsum <- result_wxsum |> #Pipe inconsistency!
  select(-all_of(wxsum_extra))

# Both below should be 0-length vectors
if (length(setdiff(colnames(result_wxsum), 
                   colnames(result_wxsumr))) > 0 ||
    length(setdiff(colnames(result_wxsumr),
                   colnames(result_wxsum))) > 0) {
  warning("Columns not identical")
}

# Now compare data; I suspect column names are in the same order, but check
which(colnames(result_wxsum) != colnames(result_wxsumr))
result_wxsum[1:5, 5:10]
result_wxsumr[1:5, 5:10]

# OK different order see if we can sort
result_wxsumr <- result_wxsumr |> 
  select(all_of(colnames(result_wxsum)))
sum(colnames(result_wxsum) != colnames(result_wxsumr))

# Now to full on comparison, but maybe base it on difference instead of ==
dif_mat <- result_wxsum - result_wxsumr
max(dif_mat)
# 27 Uh oh.
which(dif_mat > 20)

comp <- sapply(X = 1:572, FUN = function(x) {
  coldiff <- any(result_wxsum[, x] - result_wxsumr[, x] > 20)
  if (coldiff) {
    message("Big difference in ", colnames(result_wxsum)[x], " (", x, ")")
  }
})

# OK, substantial differences in multiple "dry_YYYY" columns:
# Big difference in dry_1996 (53)
# Big difference in dry_1997 (66)
# Big difference in dry_2002 (131)
# Big difference in dry_2018 (459)
# Big difference in dry_2022 (543)

# Note in all cases, the wxsum value is larger than the wxsumr value
# For 2022, the closest values were 10 days off!!! Start there.

# How well did it work in summarize_rainfall?
start_time <- Sys.time()
result_old <- summarize_rainfall(rain = rain, start_month = 5, end_month = 10, 
                                 start_day = 15, end_day = 15)
message("Completed after ", Sys.time() - start_time)
rm(start_time)

result_dry <- result_old |>
  ungroup() |>
  select(matches("dry_[0-9]{4}"))
head(result_dry)

# Old results match the wxsumr results
table(result_dry$dry_2022 - result_wxsumr$dry_2022)
# Old results DO NOT match the wxsum results
table(result_dry$dry_2022 - result_wxsum$dry_2022)
# So. How is wxsum package calculating values in the dry column?

# I think the issue might (maybe) be related to treatment of days with missing 
# values. For the dry spell calculations in wxsumr, missing days are treated 
# as rain days (so they break up the spell). Is this (still) the case with 
# wxsum/Stata?
rain_2022 <- rain |>
  select(hhid, starts_with("rf_2022")) |> 
  filter(hhid == 101)
sum(is.na(rain_2022))
# [1] 0
# Hmm...maybe not. No missing data in 2022.

# Let us try manually doing this for the first site...
rain_2022_1 <- rain_2022 |>
  select(-hhid) |>
  as.integer()
rain_binary <- as.integer(rain_2022_1 >= 1)
rain_string <- stringr::str_c(rain_binary, collapse = "")
rain_vstring <- stringr::str_c(rain_2022_1, collapse = ".")
rain_string_split <- unlist(stringr::str_split(string = rain_string,
                                               pattern = "1"))

# HAAHA! All four of the below give different answers. 
# UPDATE Not looking at the season correctly.
result_wxsumr$dry_2022[1]
result_wxsum$dry_2022[1]
max(nchar(rain_string_split))
max(nchar(rain_string_split[-c(1, length(rain_string_split))]))

# Start season: May 15, End season Oct 15. Need to subset data

rain_2022_season <- rain_2022 |>
  select(rf_20220515:rf_20221015) |>
  as.integer()
rain_binary <- as.integer(rain_2022_season >= 1)
rain_string <- stringr::str_c(rain_binary, collapse = "")
rain_string_split <- unlist(stringr::str_split(string = rain_string,
                                               pattern = "1"))
result_wxsumr$dry_2022[1]
result_wxsum$dry_2022[1]
max(nchar(rain_string_split))
max(nchar(rain_string_split[-c(1, length(rain_string_split))]))

# OK, summarize_rainfall and rain_sum were not including beginning and ending 
# stretches in the calculations. That is, if a dry spell started on day 1 or 
# ended on the last day, it was not considered as the longest dry spell. Fixed 
# the relevant code in dry_interval. Should check on behavior.

# I ran ` wxsum rf_, ini_month(05) fin_month(10) ini_day(15) fin_day(15) 

# Old below here
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
  select(-starts_with(match = "dry_start_"),
         -starts_with("dry_end_"),
         -starts_with("z_percent_raindays_"))

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

