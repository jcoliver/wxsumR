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

oddos <- grep(x = wxsum_extra,
              pattern = "__000[A-Z0-9]{3}",
              value = FALSE)
oddo_cols <- wxsum_extra[oddos]

# Drop those oddities and do the data comparison
result_wxsum <- result_wxsum |>
  select(-all_of(oddo_cols))

# As of 2026-07-08, wxsum does not include the dry_end and dry_start columns, 
# for for comparison sake, drop those for now
result_wxsumr <- result_wxsumr |>
  select(-starts_with("dry_end_")) |>
  select(-starts_with("dry_start_"))

wxsum_extra <- setdiff(colnames(result_wxsum), 
                       colnames(result_wxsumr))
wxsumr_extra <- setdiff(colnames(result_wxsumr),
                        colnames(result_wxsum))  
sort(wxsum_extra)
sort(wxsumr_extra)

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
# the relevant code in dry_interval. Should check on behavior. <--- This was it
# Current Stata code wasn't distinguishing between start, mid, or end dry 
# intervals. Per convo with JM 2026-06-11, going to keep the previous 
# functionality (start, mid, end) in the R package for now.