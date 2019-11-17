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
ids <- factor(c("Site 1", "Site 1", "Site 1", "Site 2", "Site 2", "Site 2"))
dates <- c("1983-11-30", "1984-1-30", "1984-03-30", "1983-11-30", "1984-1-30", "1984-03-30")
dates <- as.Date(dates)
values <- c(1, 1, 4, 3, 6, 9)
test_long <- data.frame(y4_hhid = ids,
                        date = dates,
                        value = values)
```

  y4_hhid       date value
1  Site 1 1983-11-30     1   # In season
2  Site 1 1984-01-30     1   # In season
3  Site 1 1984-03-30     4   # Out of season
4  Site 2 1983-11-30     3   # In season
5  Site 2 1984-01-30     6   # In season
6  Site 2 1984-03-30     9   # Out of season
