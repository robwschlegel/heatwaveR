# Detect consecutive days in exceedance above or below of a given threshold.

Detect consecutive days in exceedance above or below of a given
threshold.

## Usage

``` r
exceedance(
  data,
  x = t,
  y = temp,
  threshold,
  below = FALSE,
  minDuration = 5,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxPadLength = FALSE,
  roundRes = 4,
  returnDF = TRUE
)
```

## Arguments

- data:

  A data frame with at least the two following columns: a `t` column
  which is a vector of dates of class `Date`, and a `temp` column, which
  is the temperature on those given dates. If columns are named
  differently, their names can be supplied as `x` and `y` (see below).
  The function will not accurately detect consecutive days of
  temperatures in exceedance of the `threshold` if missing days of data
  are not filled in with `NA`. Data of the appropriate format are
  created by the internal function
  [`make_whole_fast`](https://robwschlegel.github.io/heatwaveR/index.html/reference/make_whole_fast.md),
  but your own data may be used directly if they meet the given
  criteria. Note that it is also possible to provide hourly data in the
  `x` column as class `POSIXct`.

- x:

  This column is expected to contain a vector of dates as per the
  specification of `make_whole_fast`. If a column headed `t` is present
  in the dataframe, this argument may be omitted; otherwise, specify the
  name of the column with dates here. Note that it is also possible to
  provide hourly data as class `POSIXct`.

- y:

  This is a column containing the measurement variable. If the column
  name differs from the default (i.e. `temp`), specify the name here.

- threshold:

  The static threshold used to determine how many consecutive days are
  in exceedance of the temperature of interest.

- below:

  Default is `FALSE`. When set to TRUE, consecutive days of temperature
  below the `threshold` variable are calculated. When set to FALSE,
  consecutive days above the `threshold` variable are calculated.

- minDuration:

  Minimum duration that temperatures must be in exceedance of the
  `threshold` variable. The default is `5` days.

- joinAcrossGaps:

  A TRUE/FALSE statement that indicates whether or not to join
  consecutive days of temperatures in exceedance of the `threshold`
  across a small gap between groups before/after a short gap as
  specified by `maxGap`. The default is `TRUE`.

- maxGap:

  The maximum length of the gap across which to connect consecutive days
  in exceedance of the `threshold` when `joinAcrossGaps = TRUE`.

- maxPadLength:

  Specifies the maximum length of days over which to interpolate (pad)
  missing data (specified as `NA`) in the input temperature time series;
  i.e., any consecutive blocks of NAs with length greater than
  `maxPadLength` will be left as `NA`. Set as an integer. The default is
  `3` days. Note this will be units of hours if hourly data were
  provided.

- roundRes:

  This argument allows the user to choose how many decimal places the
  exceedance metric outputs will be rounded to. Default is 4. To prevent
  rounding set `roundRes = FALSE`. This argument may only be given
  numeric values or FALSE.

- returnDF:

  The default (`TRUE`) tells the function to return the results as type
  `data.frame`. `FALSE` will return the results as a `data.table`.

## Value

The function will return a list of two data.frames. The first being
`threshold`, which shows the daily temperatures and on which specific
days the given `threshold` was exceeded. The second component of the
list is `exceedance`, which shows a medley of statistics for each
discrete group of days in exceedance of the given `threshold`. Note that
any additional columns left in the data frame given to this function
will be output in the `threshold` component of the output. For example,
if one uses
[`ts2clm`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm.md)
to prepare a time series for analysis and leaves in the `doy` column,
this column will appear in the output.

The information shown in the `threshold` component is:

- t:

  The date of the temperature measurement. This variable may named
  differently if an alternative name is supplied to the function's `x`
  argument.

- temp:

  Temperature on the specified date \[deg. C\]. This variable may named
  differently if an alternative name is supplied to the function's `y`
  argument.

- thresh:

  The static `threshold` chosen by the user \[deg. C\].

- thresh_criterion:

  Boolean indicating if `temp` exceeds `threshold`.

- duration_criterion:

  Boolean indicating whether periods of consecutive `thresh_criterion`
  are \>= `minDuration`.

- exceedance:

  Boolean indicting if all criteria that define a discrete group in
  exceedance of the `threshold` are met.

- exceedance_no:

  A sequential number indicating the ID and order of occurrence of
  exceedances.

The individual exceedances are summarised using the following metrics:

- exceedance_no:

  The same sequential number indicating the ID and order of the
  exceedance as found in the `threshold` component of the output list.

- index_start:

  Row number on which exceedance starts.

- index_peak:

  Row number on which exceedance peaks.

- index_end:

  Row number on which exceedance ends.

- duration:

  Duration of exceedance \[days\].

- date_start:

  Start date of exceedance \[date\].

- date_peak:

  Date of exceedance peak \[date\].

- date_end:

  End date of exceedance \[date\].

- intensity_mean:

  Mean intensity \[deg. C\].

- intensity_max:

  Maximum (peak) intensity \[deg. C\].

- intensity_var:

  Intensity standard deviation \[deg. C\].

- intensity_cumulative:

  Cumulative intensity \[deg. C x days\].

- rate_onset:

  Onset rate of exceedance \[deg. C / day\].

- rate_decline:

  Decline rate of exceedance \[deg. C / day\].

`intensity_max_abs`, `intensity_mean_abs`, `intensity_var_abs`, and
`intensity_cum_abs` are as above except as absolute magnitudes rather
than relative to the threshold.

## Details

1.  This function assumes that the input time series consists of
    continuous daily temperatures, with few missing values. The
    accompanying function
    [`make_whole_fast`](https://robwschlegel.github.io/heatwaveR/index.html/reference/make_whole_fast.md)
    aids in the preparation of a time series that is suitable for use
    with `exceedance`, although this may also be accomplished 'by hand'
    as long as the criteria are met as discussed in the documentation to
    [`make_whole_fast`](https://robwschlegel.github.io/heatwaveR/index.html/reference/make_whole_fast.md).

2.  Future versions seek to accommodate monthly and annual time series,
    too.

3.  The calculation of onset and decline rates assumes that exceedance
    of the `threshold` started a half-day before the start day and ended
    a half-day after the end-day. This is consistent with the duration
    definition as implemented, which assumes duration = end day - start
    day + 1.

4.  For the purposes of exceedance detection, any missing temperature
    values not interpolated over (through optional `maxPadLength`) will
    remain as `NA`. This means they will trigger the end of an
    exceedance if the adjacent temperature values are in exceedance of
    the `threshold`.

5.  If the function is used to detect consecutive days of temperature
    under the given `theshold`, these temperatures are then taken as
    being in exceedance below the `threshold` as there is no antonym in
    the English language for 'exceedance'.

This function is based largely on the `detect_event` function found in
this package, which was ported from the Python algorithm that was
written by Eric Oliver, Institute for Marine and Antarctic Studies,
University of Tasmania, Feb 2015, and is documented by Hobday et al.
(2016).

## Author

Robert W. Schlegel, Albertus J. Smit

## Examples

``` r
res <- exceedance(sst_WA, threshold = 25)
# show first ten days of daily data:
res$threshold[1:10, ]
#>             t  temp thresh threshCriterion durationCriterion exceedance
#> 1  1982-01-01 20.94     25           FALSE             FALSE      FALSE
#> 2  1982-01-02 21.25     25           FALSE             FALSE      FALSE
#> 3  1982-01-03 21.38     25           FALSE             FALSE      FALSE
#> 4  1982-01-04 21.16     25           FALSE             FALSE      FALSE
#> 5  1982-01-05 21.26     25           FALSE             FALSE      FALSE
#> 6  1982-01-06 21.61     25           FALSE             FALSE      FALSE
#> 7  1982-01-07 21.74     25           FALSE             FALSE      FALSE
#> 8  1982-01-08 21.50     25           FALSE             FALSE      FALSE
#> 9  1982-01-09 21.40     25           FALSE             FALSE      FALSE
#> 10 1982-01-10 21.36     25           FALSE             FALSE      FALSE
#>    exceedance_no
#> 1             NA
#> 2             NA
#> 3             NA
#> 4             NA
#> 5             NA
#> 6             NA
#> 7             NA
#> 8             NA
#> 9             NA
#> 10            NA
# show first five exceedances:
res$exceedance[1:5, ]
#>   exceedance_no index_start index_peak index_end duration date_start  date_peak
#> 1             1        2682       2683      2686        5 1989-05-05 1989-05-06
#> 2             2        6342       6351      6358       17 1999-05-13 1999-05-22
#> 3             3        6362       6363      6368        7 1999-06-02 1999-06-03
#> 4             4        6686       6688      6691        6 2000-04-21 2000-04-23
#> 5             5        6698       6699      6707       10 2000-05-03 2000-05-04
#>     date_end intensity_mean intensity_max intensity_var intensity_cumulative
#> 1 1989-05-09         0.2860          0.36        0.0462                 1.43
#> 2 1999-05-29         0.8559          1.40        0.3859                14.55
#> 3 1999-06-08         0.2071          0.27        0.0512                 1.45
#> 4 2000-04-26         0.1550          0.41        0.1803                 0.93
#> 5 2000-05-12         0.6970          1.01        0.2747                 6.97
#>   intensity_mean_abs intensity_max_abs intensity_var_abs intensity_cum_abs
#> 1            25.2860             25.36            0.0462            126.43
#> 2            25.8559             26.40            0.3859            439.55
#> 3            25.2071             25.27            0.0512            176.45
#> 4            25.1550             25.41            0.1803            150.93
#> 5            25.6970             26.01            0.2747            256.97
#>   rate_onset rate_decline
#> 1     0.1900       0.1171
#> 2     0.1826       0.1787
#> 3     0.1400       0.0555
#> 4     0.2540       0.1214
#> 5     0.5600       0.1371
```
