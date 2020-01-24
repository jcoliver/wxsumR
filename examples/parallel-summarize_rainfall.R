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
# With large data set, the parallel approach takes longer (203 vs. 62 seconds);
# even just the parLapply execution takes a substantial amount of time (153) 
# seconds.

# There is still a MASSIVE memory leak. After running this with the large data
# and clearing the environment with rm(list = ls()), the rsession was still 
# holding onto 3.4 GB of RAM. A call to gc() freed up a little bit (0.4 GB), but 
# still not enough.

# Try the .ls.objects function at https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# to see if there is something hanging around. Especially after an rm() call. 

# See also https://cran.r-project.org/web/packages/profmem/vignettes/profmem.html
# for using profmem

# Also in the thread of the above SO topic is this quote: 
#   "If you really want to avoid the leaks, you should avoid creating any big 
#   objects in the global environment. What I usually do is to have a function 
#   that does the job and returns NULL — all data is read and manipulated in 
#   this function or others that it calls."

library(weathercommand)
library(parallel)

# infile <- "data/input-rain-small.csv"
infile <- "data/input-rain-medium.csv"
# infile <- "data/input-rain-large.csv"

test_data <- read.csv(file = infile)

start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

########################################
# Cluster approach for parallel
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

# Apply to each row
par_start <- Sys.time()

# Have to make this a list of one-row data frames before sending to lapply
# test_list <- split(x = test_data, f = seq(nrow(test_data)), drop = TRUE)
# TODO: Look at dplyr::group_split, too
test_list <- test_data %>%
  dplyr::group_by(y4_hhid) %>%
  dplyr::group_split()

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

########################################
# Original, serial implementation
orig_start <- Sys.time()
rain_summary <- summarize_rainfall(rain = test_data,
                                   start_month = start_month,
                                   end_month = end_month,
                                   start_day = start_day,
                                   end_day = end_day,
                                   wide = FALSE)
orig_end <- Sys.time()

########################################
# Time reporting
orig_time <- difftime(time1 = orig_end, time2 = orig_start, units = "secs")
orig_time <- round(x = orig_time, digits = 3)
message(paste0("Original implementation time: ", orig_time, " seconds"))

par_time <- difftime(time1 = par_end, time2 = par_start, units = "secs")
par_time <- round(x = par_time, digits = 3)
message(paste0("Parallel implementation time: ", par_time, " seconds"))

par_sum_time <- difftime(time1 = par_sum_end, time2 = par_sum_start, units = "secs")
par_sum_time <- round(x = par_sum_time, digits = 3)
message(paste0("Time for parLapply call: ", par_sum_time, " seconds"))
message(paste0("Additional parallel processing time (incl. split call): ", 
               round(par_time - par_sum_time, digits = 3), " seconds"))
