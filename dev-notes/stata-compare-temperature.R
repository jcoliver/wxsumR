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
# start_month <- 11
# end_month <- 2
# start_day <- 1
# end_day <- start_day
# growbase_low <- 8
# growbase_high <- 32
# kdd_threshold <- 32
# bins <- 4
# id_index <- 1
# date_sep <- "_"
# complete_only <- TRUE
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

# For now, remove hhid, which is site id
samecol_wxsum <- result_wxsum |>
  select(-all_of(wxsum_extra))
samecol_wxsumr <- result_wxsumr |>
  select(-all_of(wxsumr_extra))

# Get the columns in the same order
samecol_wxsumr <- samecol_wxsumr |>
  select(colnames(samecol_wxsum))

dif_mat <- samecol_wxsum - samecol_wxsumr
max(dif_mat, na.rm = TRUE) # 0.00068
# Calculate RMSEs for some plotting
col_rmse <- sqrt(colMeans(dif_mat^2))
# sort(col_rmse)

# Want to do some summary visualization, so make a data frame of those rmses
col_rmse <- as.data.frame(col_rmse)
col_rmse$names <- rownames(col_rmse)
rownames(col_rmse) <- NULL

col_rmse$names <- gsub(x = col_rmse$names,
                        pattern = "_[0-9]{4}",
                        replacement = "")
col_rmse$names <- gsub(x = col_rmse$names,
                        pattern = "tempbin_[0-9]{2}",
                        replacement = "tempbin")
ggplot(data = col_rmse,
       mapping = aes(x = names, y = col_rmse)) +
  geom_point()

# It's dev_gdd and gdd values that the most divergent (diff 1e-04 to 
# 2.5e-04). z_gdd is also a little high rmse (2e-06 to 5e-06).

# Look at just those three
ggplot(data = col_rmse |> 
         filter(names %in% c("dev_gdd", "gdd", "z_gdd")),
         mapping = aes(x = names, y = col_rmse)) +
  geom_point()


# Plotting the columns without the three gdd columns, we see the remaining 
# columns all have rmse less than 1e-06.
ggplot(data = col_rmse |> 
         filter(!(names %in% c("dev_gdd", "gdd", "z_gdd"))),
       mapping = aes(x = names, y = col_rmse)) +
  geom_point()

# We want to do a "manual" calculation of some binmean values. What columns are 
# we going to need to work with?
# binmean_01 is the mean value for temperature bin 1, across years. Let's pull
# out all tempbin_01 values across all years. Work from result_wxsum/r, as 
# those still have the id column
wxsum_tempbin_01 <- result_wxsum |>
  select(hhid, starts_with("tempbin_01"))
wxsumr_tempbin_01 <- result_wxsumr |>
  select(hhid, starts_with("tempbin_01"))
# First of all, values in wxsumr results include 1992 & 2023, which they should 
# not (?)
# Start by just doing mean calculations across all the columns, and compare 
# those values to the results of binmean_01
wxsum_tempbin_01 <- wxsum_tempbin_01 |>
  rowwise() |>
  mutate(binmean_01 = mean(c_across(starts_with("tempbin_01")))) |>
  ungroup()
wxsumr_tempbin_01 <- wxsumr_tempbin_01 |>
  rowwise() |>
  mutate(binmean_01 = mean(c_across(starts_with("tempbin_01")))) |>
  ungroup()

# OK, the manual mean calculation (above) matches what we got out of the 
# results. (differences below are zero)
wxsum_binvar_01_d <- wxsum_tempbin_01$binmean_01 - result_wxsum$binmean_01
wxsumr_binvar_01_d <- wxsumr_tempbin_01$binmean_01 - result_wxsumr$binmean_01

# Now see what happens when we drop 1992 & 2023 from the wxsumr version
wxsumr_tempbin_01 <- wxsumr_tempbin_01 |>
  select(-c(tempbin_01_1992, tempbin_01_2023)) |>
  rowwise() |>
  mutate(binmean_01_a = mean(c_across(starts_with("tempbin_01")))) |>
  ungroup()

# Differences below show that this was the issue. Dropping 1992 and 2023 from 
# binmean calculation (second difference below) gives same values that are 
# coming from the Stata version calculations
wxsumr_binvar_01_d <- wxsumr_tempbin_01$binmean_01_a - result_wxsumr$binmean_01
wxsum_v_wxsumr_binvar_01_d <- wxsumr_tempbin_01$binmean_01_a - result_wxsum$binmean_01

