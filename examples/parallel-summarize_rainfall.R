# Attempt to parallize rain summary
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-08

rm(list = ls())

################################################################################
# Remember https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
# With small data set, parallel takes longer (0.8 vs. 2.4 seconds).
# With medium data set, parallel still takes longer (10.7 vs. 22.8 seconds); a 
# substantial part of this is setting up the cluster and breaking the data into 
# a list object (9.9 seconds), but even just the parallel execution of 
# summarize_rainfall takes 12.8 seconds. Consider a dplyr analog to split.
# Also test with large data. If original implementation works and still outpaces
# parallel approach, probably not worth it to parallelize
library(weathercommand)
library(parallel)

# infile <- "data/input-rain-small.csv"
infile <- "data/input-rain-medium.csv"
outfile <- NULL

test_data <- read.csv(file = infile)

start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

# Try just using lapply
# Have to make this a list of one-row data frames before sending to lapply
# TODO: Look at dplyr::group_split, too
test_list <- split(x = test_data, f = seq(nrow(test_data)), drop = TRUE)
# test_list <- test_data %>%
#   dplyr::group_by(y4_hhid) %>%
#   dplyr::group_split()
# With medium sized data, this is throwing the warning 
# Factor `y4_hhid` contains implicit NA, consider using `forcats::fct_explicit_na`
# It is being thrown in the first set of summary calculations in 
# summarize_rainfall, probably at this line:
# dplyr::group_by(season_year, !!as.name(id_column_name)) %>%

# Attempt to fix by dropping unused levels in summarize_rainfall via 
# rain_long[, id_column_name] <- droplevels(x = rain_long[, id_column_name])
# Did not solve this problem
lapply_summary <- lapply(X = test_list,
                         FUN = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)
rain_summary <- dplyr::bind_rows(lapply_summary)

# Trying parts of the medium-sized data to see if it is a size or data problem
# 1-100 works, 101-200 fails, 201-300 fails, see more below
options(warn = 2) # So warnings become errors to make tracebacks possible
for (i in seq(from = 0, to = 900, by = 100)) {
  row_start <- i + 1
  row_end <- i + 100
  message(paste0("Testing rows ", row_start, " to ", row_end, "..."))
  # Half of the subsets throw errors: 101-200, 201-300, ...
  if (!(i %in% c(100, 200, 500, 700, 900))) {
    subset_data <- test_data[row_start:row_end, ]
    subset_list <- split(x = subset_data, f = seq(nrow(subset_data)), drop = TRUE)
    lapply_summary <- lapply(X = subset_list,
                             FUN = summarize_rainfall,
                             start_month = start_month,
                             end_month = end_month,
                             start_day = start_day,
                             end_day = end_day,
                             wide = FALSE)
    message(paste0("...test ", row_start, "-", row_end, " complete."))
  } else {
    message(paste0("..skipping ", row_start, "-", row_end, "."))
  }
}

# Trying to identify where the error occurs in the 101-200 subset
# Occurs in 181-190; Row 182 has NA for all data columns
for (i in seq(from = 100, to = 200, by = 10)) {
  row_start <- i + 1
  row_end <- i + 10
  message(paste0("Testing rows ", row_start, " to ", row_end, "..."))
  # Half of the subsets throw errors: 101-200, 201-300, ...
  if (!(i %in% c(180, 2000))) {
    subset_data <- test_data[row_start:row_end, ]
    subset_list <- split(x = subset_data, f = seq(nrow(subset_data)), drop = TRUE)
    lapply_summary <- lapply(X = subset_list,
                             FUN = summarize_rainfall,
                             start_month = start_month,
                             end_month = end_month,
                             start_day = start_day,
                             end_day = end_day,
                             wide = FALSE)
    message(paste0("...test ", row_start, "-", row_end, " complete."))
  } else {
    message(paste0("..skipping ", row_start, "-", row_end, "."))
  }
}

# Trying to identify where the error occurs in the 201-300 subset to see if 
# there is a similar row filled with NAs
# Yes, row 214 has NA all the way across
for (i in seq(from = 200, to = 300, by = 10)) {
  row_start <- i + 1
  row_end <- i + 10
  message(paste0("Testing rows ", row_start, " to ", row_end, "..."))
  # Half of the subsets throw errors: 101-200, 201-300, ...
  if (!(i %in% c(210, 2000))) {
    subset_data <- test_data[row_start:row_end, ]
    subset_list <- split(x = subset_data, f = seq(nrow(subset_data)), drop = TRUE)
    lapply_summary <- lapply(X = subset_list,
                             FUN = summarize_rainfall,
                             start_month = start_month,
                             end_month = end_month,
                             start_day = start_day,
                             end_day = end_day,
                             wide = FALSE)
    message(paste0("...test ", row_start, "-", row_end, " complete."))
  } else {
    message(paste0("..skipping ", row_start, "-", row_end, "."))
  }
}
options(warn = 0) # reset to default behavior of warnings

# Test to see what happens with non-apply (original) approach with those 
# rows of all missing values
rain_summary <- summarize_rainfall(rain = test_data[211:220, ],
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)

# In the offending row (), y4_hhid == 1499-001, does not end up in the output, 
# so all NA data end up getting dropped (silently) when using this approach

# What happens when ONLY that row is sent to summarize_rainfall?
rain_summary <- summarize_rainfall(rain = test_data[214, ],
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
# We get that warning message about implicit NAs. There is a good chance that 
# by the time the summary calculations start, the data has zero rows...
# Test parts of summarize_rainfall to see where that happens

# Copied from summarize_rainfall and modified for use here
# Use to_long to convert to long format and parse column names into dates
rain_long <- weathercommand:::to_long(data = test_data[214, ]) # works fine

# Exclude NA dates
library(magrittr)
rain_long <- rain_long %>%
  tidyr::drop_na(date) # works fine, too. All rows still there

# Enumerate seasons <- This drops all rows because each row has an NA value
rain_long <- weathercommand:::enumerate_seasons(data = rain_long,
                                                start_month = start_month,
                                                end_month = end_month,
                                                start_day = start_day,
                                                end_day = end_day)

# Assume first column has site id
id_column_name <- colnames(rain_long)[1]

# Start with calculating basic statistics, including the longest number of
# consecutive days without rain in the period
na.rm <- TRUE
rain_cutoff <- 1.5
rain_summary <- rain_long %>%
  dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
  dplyr::summarize(mean_season = mean(x = value, na.rm = na.rm),
                   dry = weathercommand:::dry_interval(x = value, rain_cutoff = rain_cutoff, period = "mid", na.rm = na.rm))
  

# Close, but in this case, the value of dry returned is 0 (for every year). It 
# should be NA
# UPDATE: Works as expected, after adding intial check to dry_interval for 
# vector of all NA values

# Try on row of _good_ data to make sure it works
rain_long <- weathercommand:::to_long(data = test_data[213, ])

# Exclude NA dates
library(magrittr)
rain_long <- rain_long %>%
  tidyr::drop_na(date) # works fine, too. All rows still there

# Enumerate seasons <- This drops all rows because each row has an NA value
rain_long <- weathercommand:::enumerate_seasons(data = rain_long,
                                                start_month = start_month,
                                                end_month = end_month,
                                                start_day = start_day,
                                                end_day = end_day)

# Assume first column has site id
id_column_name <- colnames(rain_long)[1]

# Start with calculating basic statistics, including the longest number of
# consecutive days without rain in the period
na.rm <- TRUE
rain_cutoff <- 1.5
rain_summary <- rain_long %>%
  dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
  dplyr::summarize(mean_season = mean(x = value, na.rm = na.rm),
                   dry = weathercommand:::dry_interval(x = value, rain_cutoff = rain_cutoff, period = "mid", na.rm = na.rm))


########################################
# Set up the cluster for parallel processing
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

# Apply to each row
par_start <- Sys.time()
test_list <- split(x = test_data, f = seq(nrow(test_data)), drop = TRUE)
par_sum_start <- Sys.time()
par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)
par_sum_end <- Sys.time()
stopCluster(cl = clust)
rain_summary <- dplyr::bind_rows(par_summary)
par_end <- Sys.time()

orig_start <- Sys.time()
rain_summary <- summarize_rainfall(rain = test_data,
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
orig_end <- Sys.time()

orig_time <- difftime(time1 = orig_end, time2 = orig_start, units = "secs")
orig_time <- round(x = orig_time, digits = 3)
message(paste0("Original implementation time: ", orig_time, " seconds"))

par_time <- difftime(time1 = par_end, time2 = par_start, units = "secs")
par_time <- round(x = par_time, digits = 3)
message(paste0("Parallel implementation time: ", par_time, " seconds"))

par_sum_time <- difftime(time1 = par_sum_end, time2 = par_sum_start, units = "secs")
par_sum_time <- round(x = par_sum_time, digits = 3)
message(paste0("Time for parLapply call: ", par_sum_time, " seconds"))
message(paste0("Additional parallel processing time (incl. split call): ", (par_time - par_sum_time), " seconds"))
