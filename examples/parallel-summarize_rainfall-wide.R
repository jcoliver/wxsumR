# Debug the wide-format option of parallel approach to summarize_rainfall
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2020-01-08

rm(list = ls())

################################################################################
library(weathercommand)
library(parallel)
library(tidyverse)

infile <- "data/input-rain-small.csv"
# infile <- "data/input-rain-medium.csv"
# infile <- "data/input-rain-large.csv"

test_data <- read.csv(file = infile)

start_month <- 11
end_month <- 02
start_day <- 15
end_day <- 25

####################
# Wide formatted output
wide <- TRUE
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

# Create indicator by which to split
split_var <- sort(rep(x = 1:num_cores, length = nrow(test_data)))
test_data$split_var <- split_var[1:nrow(test_data)]

# Create a list, which is needed by parLapply
test_list <-  test_data %>%
  dplyr::group_by(split_var) %>%
  dplyr::group_split()

par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = wide)

stopCluster(cl = clust)

# Bind results back together in a tibble
rain_summary_smart_par <- dplyr::bind_rows(par_summary)

# For consistency with serial output, need to re-order rows
if ("season_year" %in% colnames(rain_summary_smart_par)) {
  # Long format, re-order by season_year, then id column
  rain_summary_smart_par <- rain_summary_smart_par %>%
    arrange(season_year, y4_hhid)
} else {
  # Wide format, re-order only by id column
  rain_summary_smart_par <- rain_summary_smart_par %>%
    arrange(y4_hhid)
}

wide_summary <- rain_summary_smart_par

################################################################################
# Do it again with long-format output
wide = FALSE
num_cores <- detectCores() - 1
clust <- makeCluster(num_cores)

# Need to explicitly make weathercommand available on each node
clusterEvalQ(clust, library(weathercommand))

# Create indicator by which to split
split_var <- sort(rep(x = 1:num_cores, length = nrow(test_data)))
test_data$split_var <- split_var[1:nrow(test_data)]

# Create a list, which is needed by parLapply
test_list <-  test_data %>%
  dplyr::group_by(split_var) %>%
  dplyr::group_split()

par_summary <- parLapply(cl = clust,
                         X = test_list,
                         fun = summarize_rainfall,
                         start_month = start_month,
                         end_month = end_month,
                         start_day = start_day,
                         end_day = end_day,
                         wide = wide)

stopCluster(cl = clust)

# Bind results back together in a tibble
rain_summary_smart_par <- dplyr::bind_rows(par_summary)

# For consistency with serial output, need to re-order rows
if ("season_year" %in% colnames(rain_summary_smart_par)) {
  # Long format, re-order by season_year, then id column
  rain_summary_smart_par <- rain_summary_smart_par %>%
    arrange(season_year, y4_hhid)
} else {
  # Wide format, re-order only by id column
  rain_summary_smart_par <- rain_summary_smart_par %>%
    arrange(y4_hhid)
}

long_summary <- rain_summary_smart_par
