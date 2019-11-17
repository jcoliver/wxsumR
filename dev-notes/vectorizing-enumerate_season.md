Notes for vectorizing enumerate_seasons
2019-11-17

We ultimately want a column `season_year` that indicates the "season year" the
observation corresponds to. For seasons that do not include the new year, this
is straightforward: `season_year` is the year from the `date` column. _However_
if the season includes the new year, e.g. season is 15 November through 15
February, it requires a little more information. We are using the year the
season _starts in_ as the value to use for `season_year`. So for a season
defined by 15 November 1983 - 15 February 1984, the value for `season_year` will
be 1983.

```
ids <- factor(c("Site 1", "Site 1", "Site 1", "Site 1", "Site 1", "Site 1"))
dates <- c("1983-11-30", "1984-1-30", "1984-03-30", "1984-11-30", "1985-1-30", "1985-03-30")
dates <- as.Date(dates)
values <- c(1, 1, 4, 3, 6, 9)
test_long <- data.frame(y4_hhid = ids,
                        date = dates,
                        value = values)
```

start_month is 11
end_month is 02
day is 15

OBSY is year of the observation

## Case 1
start_month is 11
end_month is 02
day is 15
1983-11-30     # In season
+ greater than OBSY-11-15
+ greater than OBSY-02-15
+ season_year is OBSY
1984-01-30     # In season
+ less than OBSY-11-15
+ less than OBSY-02-15
+ season_year is OBSY - 1
1984-03-30     # Out of season
+ less than OBSY-11-15
+ greater than OBSY-02-15

## Case 2
start_month is 02
end_month is 11
day is 15
1983-11-30     # Out of season
+ greater than OBSY-11-15
+ greater than OBSY-02-15
1984-01-30     # Out of season
+ less than OBSY-11-15
+ less than OBSY-02-15
1984-03-30     # In season
+ less than OBSY-11-15
+ greater than OBSY-02-15
+ season_year is OBSY
