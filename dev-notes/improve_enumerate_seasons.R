# Testing improvements to enumerate_seasons
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-08-20

devtools::load_all() # to make sure the package is loaded

rain <- readRDS(file = "data/rain.rds")

rain_long <- wxsumR:::to_long(data = rain)

start_month <- 10 # 5
end_month <- 12  # 10
start_day <- 15 
end_day <- 15

x <- rain_long$date
# This is from season_year (in the R/enumerate_seasons.R file):

# Will hold return value (sy = season year)
sy <- rep(NA, length(x))

# The year of observation in x
obs_year <- lubridate::year(x)

# Starting and ending dates based on x's year

num_reps <- 100
orig <- numeric(num_reps)
sep_arg <- numeric(num_reps)
format_arg <- numeric(num_reps)
for (i in 1:num_reps) {
  if (i %% 5 == 0) {
    message("Rep ", i)
  }
  before <- Sys.time()
  start_OBSY <- lubridate::as_date(stringr::str_c(obs_year, "-", start_month, "-", start_day))
  end_OBSY <- lubridate::as_date(stringr::str_c(obs_year, "-", end_month, "-", end_day))
  # message("Original calcs: ", Sys.time() - before)
  orig[i] <- Sys.time() - before
  
  before <- Sys.time()
  start_OBSY <- lubridate::as_date(stringr::str_c(obs_year, start_month, start_day, sep = "-"))
  end_OBSY <- lubridate::as_date(stringr::str_c(obs_year, end_month, end_day, sep = "-"))
  # message("Use sep argument: ", Sys.time() - before)
  sep_arg[i] <- Sys.time() - before
  
  before <- Sys.time()
  start_OBSY <- lubridate::as_date(stringr::str_c(obs_year, 
                                                  sprintf("%02d", start_month), 
                                                  sprintf("%02d", start_day)),
                                   format = "%Y%m%d")
  end_OBSY <- lubridate::as_date(stringr::str_c(obs_year, 
                                                sprintf("%02d", end_month), 
                                                sprintf("%02d", end_day)),
                                 format = "%Y%m%d")
  # message("Use format argument: ", Sys.time() - before)
  format_arg[i] <- Sys.time() - before
}

mean(orig)
mean(sep_arg)
mean(format_arg)

# Logic for extracting season year depends on whether or not season includes
# the new year
if (start_month > end_month) {
  # Season INCLUDES the new year
  
  # If x occurs after both start and end for year of observation, it is in
  # season and its season_year is just the year of the observation
  sy[x >= start_OBSY & x >= end_OBSY] <- obs_year[x >= start_OBSY & x >= end_OBSY]
  
  # If x occurs before both start and end for the year of observation, it is
  # in season and its season_year is the year *prior* to the year of the
  # observation
  sy[x < start_OBSY & x <= end_OBSY] <- obs_year[x < start_OBSY & x <= end_OBSY] - 1
} else {
  # Season EXCLUDES the new year
  
  # If observation is after start in year of observation and before end in
  # year of observation, it is in season and season_year is the year of the
  # observation
  sy[x >= start_OBSY & x <= end_OBSY] <- obs_year[x >= start_OBSY & x <= end_OBSY]
}
