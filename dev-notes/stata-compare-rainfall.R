# Compare rainfall output with STATA output
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-04-06

library(haven)  # to read output of wxsum (STATA) output
library(dplyr)  # some data wrangling fun 
library(ggplot2) # visualizating differences

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
result_wxsumr <- summarize_rainfall(rain = rain, start_month = 5, end_month = 10, 
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

# Now compare data; start by arranging columns to same order
result_wxsumr <- result_wxsumr |> 
  select(all_of(colnames(result_wxsum)))
sum(colnames(result_wxsum) != colnames(result_wxsumr))

# Now to full on comparison, but maybe base it on difference instead of ==
dif_mat <- result_wxsum - result_wxsumr
max(dif_mat) # 4.88e-05

comp <- sapply(X = 1:572, FUN = function(x) {
  coldiff <- any(result_wxsum[, x] - result_wxsumr[, x] > 20)
  if (coldiff) {
    message("Big difference in ", colnames(result_wxsum)[x], " (", x, ")")
  }
})

# Do RMSE calcs and plot
col_rmse <- sqrt(colMeans(dif_mat^2))
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
  geom_point() + 
  coord_flip()

# Big diffs are dev_total, mean_mo_total, sd_mo_total (RMSE around 2e-05 for 
# dev, 5e-06 for *_mo_total), look at just those three
ggplot(data = col_rmse |> 
         filter(names %in% c("dev_total", "mean_mo_total", "sd_mo_total")),
       mapping = aes(x = names, y = col_rmse)) +
  geom_point() +
  coord_flip()

# Without those three, highest are less than 3e-06, most are below 1e-06
ggplot(data = col_rmse |> 
         filter(!(names %in% c("dev_total", "mean_mo_total", "sd_mo_total"))),
       mapping = aes(x = names, y = col_rmse)) +
  geom_point() +
  coord_flip()
