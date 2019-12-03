# Conversion notes

## 2019-12-02
Move large files out of folder under version control. i.e. moved to 
../large-data-files

## 2019-11-07
Use NPSY4 data files for testing. They don't have the idiosyncracies as in the
ihs4p data.

## 2019-11-06
In the ihs4p_x0_rf1_daily.csv data, some columns have invalid dates, e.g. (3rd 
and 4th columns invalid dates):
  rf_19830227 rf_19830228 rf_19830229 rf_19830230 rf_19830301
1           0          10           0           0           0
2           0          10           0           0           0
3           0          10           0           0           0
4           0          10           0           0           0

Looks like each year has values for:
29 February (only an issue for common years)
30 February
31 April
31 June
But not values for:
31 September
31 November

`to_long` sets these as `NA` values. For now, just exclude before 
`enumerate_seasons` is called

Also, each year only has data from 01 January to 31 August

## 2019-11-04
Data coming in are wide, where:

+ Each row is a location
+ Each column is a day, specified with column name <y>_YYYYMMDD, where <y> is generally an indicator of the type of data, e.g. rain or temperature
+ Data are measured daily

E.g. 
   y3_hhid rf_19830101 rf_19830102 rf_19830103 rf_19830104
1 0001-002           0           0           0           0
2 0003-001           0           0           0           0
3 0003-003           0           0           0           0
4 0015-001           0           0           0           0
5 0004-001           0           0           0           0

Generally a choice of either doing rain or temperature. This should be dictated by the function chosen, _not_ an argument passed to a function

Summary stats are created for a **season**, defined a priori through starting month, ending month, and day of month. Some challenges with this:

+ Need to be able to accommodate seasons that span year boundaries, e.g. Nov - Mar.
+ Day of month has potentially NA issues (i.e. no February 31)

Will probably need to convert to long and parse column names into three separate columns: year, month, day

Output example [1:5, 1:5] of data/results_rain.csv:
   y3_hhid mean_season_1983 median_season_1983 sd_season_1983 total_season_1983
1 0001-002         10.20635                 10       8.187635               643
2 0003-001         10.20635                 10       8.187635               643
3 0003-003         10.20635                 10       8.187635               643
4 0015-001         10.20635                 10       8.187635               643
5 0004-001         10.20635                 10       8.187635               643

Biggest issue will be enumerating each "season"

Start with:

Mean daily rainfall
Median daily rainfall
Standard deviation of daily rainfall