# Benchmarking components of enumerate_rainfall for big data solution
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2019-12-13

# Benchmarking the time it takes to perform calculations with different data
# sets. The time processes take to complete appear to scale linearly with
# with increasing size of data in either dimension;
# i.e. 10 times more rows -> 10 times longer
# 5 times more columns -> 5 times longer
# No obvious RAM spikes in any of the processes
# But they are all operating serially, so there is opportunity for
# parallelization
# With large data, RAM increases at to_long, releasing a little bit back at the
# end, increases again with enumerate_seasons, giving a little back at the end,
# then takes a little more end performing summary calculations of
# enumerate_rainfall; RAM is *not* given back, even when large data objects are
# removed from memory via `rm`. Only when R was restarted was the RAM
# released. ***MEMORY LEAK***
# See http://adv-r.had.co.nz/memory.html for potential solutions

rm(list = ls())

################################################################################
library(weathercommand)
library(dplyr)
library(tidyr)

input_files <- c("data/input-rain-small.csv",
                 "data/input-rain-medium.csv",
                 "data/input-rain-medium-wider.csv",
                 "data/input-rain-large.csv")

outputs <- data.frame(infile = input_files,
                      to_long = NA,
                      enum_season = NA,
                      summ_rain = NA,
                      calc_1 = NA,
                      calc_2 = NA,
                      calc_3 = NA,
                      to_wide = NA,
                      total_time = NA)

for (infile in input_files) {
  Sys.sleep(time = 5) # 5 second sleep
  cat(paste0("Input file: ", infile, "\n"))
  i <- which(input_files == infile)

  orig_data <- read.csv(file = infile)

  ########################################
  # to_long

  cat("Converting to long...\n")
  long_start <- Sys.time()
  long_data <- weathercommand:::to_long(data = orig_data)

  # Exclude NA dates
  long_data <- long_data[!is.na(long_data$date), ]
  long_end <- Sys.time()

  ########################################
  # enumerate_seasons

  cat("Enumerating seasons...\n")
  enum_start <- Sys.time()
  start_month <- 11
  end_month <- 2
  long_data <- weathercommand:::enumerate_seasons(data = long_data,
                                                  start_month = start_month,
                                                  end_month = end_month)
  enum_end <- Sys.time()

  ########################################
  # summarize_rainfall

  summ_start <- Sys.time()
  # Assume first column has site id
  id_column_name <- colnames(long_data)[1]
  na_rm <- TRUE
  rain_cutoff <- 1
  wide <- TRUE

  cat("First summary calculations...\n")
  summ_first <- Sys.time()
  # Start with calculating basic statistics, including the longest number of
  # consecutive days without rain in the period
  data_summary <- long_data %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::summarize(mean_season = mean(x = value, na.rm = na_rm),
                     median_season = median(x = value, na.rm = na_rm),
                     sd_season = sd(x = value, na.rm = na_rm),
                     total_season = sum(x = value, na.rm = na_rm),
                     skew_season = (mean(x = value, na.rm = na_rm) - median(x = value, na.rm = na_rm))/sd(x = value, na.rm = na_rm),
                     norain = sum(x = value < rain_cutoff, na.rm = na_rm),
                     raindays = sum(x = value >= rain_cutoff, na.rm = na_rm),
                     raindays_percent = sum(x = value >= rain_cutoff, na.rm = na_rm)/dplyr::n(),
                     dry = weathercommand:::dry_interval(x = value, rain_cutoff = rain_cutoff, period = "mid"),
                     dry_start = weathercommand:::dry_interval(x = value, rain_cutoff = rain_cutoff, period = "start"),
                     dry_end = weathercommand:::dry_interval(x = value, rain_cutoff = rain_cutoff, period = "end"))

  cat("Second summary calculations...\n")
  summ_second <- Sys.time()

  # Add long-term values mean and standard-deviation values
  data_summary <- dplyr::ungroup(data_summary) %>%
    dplyr::group_by(!!as.name(id_column_name)) %>%
    dplyr::mutate(mean_period_total_season = mean(x = total_season),
                  sd_period_total_season = sd(x = total_season),
                  mean_period_norain = mean(x = norain),
                  sd_period_norain = sd(x = norain),
                  mean_period_raindays = mean(x = raindays),
                  sd_period_raindays = sd(x = raindays),
                  mean_period_raindays_percent = mean(x = raindays_percent),
                  sd_period_raindays_percent = sd(x = raindays_percent))

  cat("Third summary calculations...\n")
  summ_third <- Sys.time()

  # Finally, calculate deviations as deviations from the mean; for total_season,
  # also report as a z-score
  data_summary <- dplyr::ungroup(data_summary) %>%
    dplyr::group_by(season_year, !!as.name(id_column_name)) %>%
    dplyr::mutate(dev_total_season = total_season - mean_period_total_season,
                  z_total_season = (total_season - mean_period_total_season)/sd_period_total_season,
                  dev_raindays = raindays - mean_period_raindays,
                  dev_norain = norain - mean_period_norain,
                  dev_raindays_percent = raindays_percent - mean_period_raindays_percent)

  cat("Converting to wide...\n")
  summ_fourth <- Sys.time()

  if (wide) {
    # Long-term columns won't be separated out into separate columns for each
    # year
    long_term_cols <- c("mean_period_total_season", "sd_period_total_season",
                        "mean_period_norain", "sd_period_norain",
                        "mean_period_raindays", "sd_period_raindays",
                        "mean_period_raindays_percent", "sd_period_raindays_percent")
    data_summary <- weathercommand:::wide_summary(x = data_summary,
                                                  id_col = id_column_name,
                                                  long_term_cols = long_term_cols)
  }

  cat("Converting output to data.frame...\n")
  summ_fifth <- Sys.time()

  data_summary <- as.data.frame(data_summary)

  summ_end <- Sys.time()
  ########################################

  outputs$to_long[i] <- difftime(long_end, long_start, units = "secs")
  outputs$enum_season[i] <- difftime(enum_end, enum_start, units = "secs")
  outputs$summ_rain[i] <- difftime(summ_end, summ_start, units = "secs")
  outputs$calc_1[i] <- difftime(summ_second, summ_first, units = "secs")
  outputs$calc_2[i] <- difftime(summ_third, summ_second, units = "secs")
  outputs$calc_3[i] <- difftime(summ_fourth, summ_third, units = "secs")
  outputs$to_wide[i] <- difftime(summ_fifth, summ_fourth, units = "secs")
  outputs$total_time[i] <- difftime(summ_end, long_start, units = "secs")

  infile_col <- which(colnames(outputs) == "infile")
  outputs[, -infile_col] <- round(x = outputs[, -infile_col], digits = 3)

  # Get rid of larger data objects
  rm(data_summary, long_data, orig_data)
  # message("Process complete. Reported times:")
  # message(paste0("to_long: ", long_end - long_start))
  # message(paste0("enumerate_seasons: ", enum_end - enum_start))
  # message(paste0("summarize_rainfall total time: ", summ_end - summ_start))
  # message(paste0("\tFirst set of calculations: ", summ_second - summ_first))
  # message(paste0("\tSecond set of calculations: ", summ_third - summ_second))
  # message(paste0("\tThird set of calculations: ", summ_fourth - summ_third))
  # message(paste0("\tConversion to wide (if applicable): ", summ_fifth - summ_fourth))
}
