# Attempt to parallize rain summary
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-08

rm(list = ls())

################################################################################
# Remember https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
# With small data set, parallel takes longer (0.8 vs. 2.4 seconds). See what 
# happens with larger data set
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

# Set up the cluster for parallel processing
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

# Apply to each row
par_start <- Sys.time()
test_list <- split(x = test_data, f = seq(nrow(test_data)), drop = TRUE)
par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = FALSE)
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
