# Make a climatology from a daily time series.

This is the fully data.table-based version of
[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).
The function creates a daily climatology from a time series of daily
temperatures using a user-specified sliding window for the mean and
threshold calculation, followed by an optional moving average smoother
as used by Hobday et al. (2016).

## Usage

``` r
ts2clm3(
  data,
  x = t,
  y = temp,
  climatologyPeriod,
  maxPadLength = FALSE,
  windowHalfWidth = 5,
  pctile = 90,
  smoothPercentile = TRUE,
  smoothPercentileWidth = 31,
  clmOnly = FALSE,
  var = FALSE,
  roundClm = 4,
  ...
)
```

## Arguments

- data:

  A data frame with at least two columns. In the default setting (i.e.
  omitting the arguments `x` and `y`; see immediately below), the data
  set is expected to have the headers `t` and `temp`. The `t` column is
  a vector of dates of class `Date`, while `temp` is the measured
  variable (by default it is assumed to be temperature). Any additional
  columns are not used in calculations but will be correctly carried
  over into the output climatology. Such additional columns may be site
  names, longitudes, or latitudes (etc.), but note that they are not
  meant to specify a grouping structure of the time series. As such,
  grouping structures are not handled by the function in its current
  form, and a time series is therefore assumed to be located at a
  discrete point in space such as one 'pixel' of longitude × latitude as
  one might find in gridded data products.

- x:

  This column is expected to contain a vector of dates. If a column
  headed `t` is present in the data frame, this argument may be omitted;
  otherwise, specify the name of the column with dates here.

- y:

  This is a column containing the measurement variable. If the column
  name differs from the default (i.e. `temp`), specify the name here.

- climatologyPeriod:

  Required. To this argument should be passed two values (see example
  below). The first value should be the chosen date for the start of the
  climatology period, and the second value the end date of said period.
  This chosen period (preferably 30 years in length) is then used to
  calculate the seasonal cycle and the extreme value threshold.

- maxPadLength:

  Specifies the maximum length of days over which to apply linear
  interpolation (padding) across the missing values (specified as `NA`)
  in the measured variable; i.e., any consecutive blocks of NAs with
  length greater than `maxPadLength` will be left as `NA`. The default
  is `FALSE`. Set as an integer to interpolate. Setting `maxPadLength`
  to `TRUE` will return an error.

- windowHalfWidth:

  Width of sliding window about day-of-year (to one side of the center
  day-of-year) used for the pooling of values and calculation of
  climatology and threshold percentile. Default is `5` days, which gives
  a window width of 11 days centred on the 6th day of the series of 11
  days.

- pctile:

  Threshold percentile (%) for detection of events (MHWs). Default is
  `90`th percentile. Should the intent be to use these threshold data
  for MCSs, set `pctile = 10` or some other low value.

- smoothPercentile:

  Boolean. Select whether to smooth the climatology and threshold
  percentile time series with a moving average of
  `smoothPercentileWidth`. The default is `TRUE`.

- smoothPercentileWidth:

  Full width of moving average window for smoothing the climatology and
  threshold. The default is `31` days.

- clmOnly:

  Boolean. Choose to calculate and return only the climatologies. The
  default is `FALSE`.

- var:

  Boolean. This argument has been introduced to allow the user to choose
  if the variance of the seasonal signal per doy should be calculated.
  The default of `FALSE` will prevent the calculation. Setting it to
  `TRUE` might potentially increase the speed of calculations on gridded
  data and increase the size of the output. The variance was initially
  introduced as part of the standard output from Hobday et al. (2016),
  but few researchers use it and so it is generally regarded now as
  unnecessary.

- roundClm:

  This argument allows the user to choose how many decimal places the
  `seas` and `thresh` outputs will be rounded to. Default is 4. To
  prevent rounding set `roundClm = FALSE`. This argument may only be
  given numeric values or FALSE.

- ...:

  Allows unused arguments to pass through the functions.

## Value

The function will return a data.table (see the `data.table`) with the
input time series and the newly calculated climatology. The climatology
contains the daily climatology and the threshold for calculating MHWs.
The software was designed for creating climatologies of daily
temperatures, and the units specified below reflect that intended
purpose. However, various other kinds of climatologies may be created,
and if that is the case, the appropriate units need to be determined by
the user.

- doy:

  Julian day (day-of-year) returned when `clmOnly = TRUE`. For non-leap
  years it runs 1...59 and 61...366, while leap years run 1...366.

- t:

  The date vector in the original time series supplied in `data`. If an
  alternate column was provided to the `x` argument, that name will
  rather be used for this column.

- temp:

  The measurement vector as per the the original `data` supplied to the
  function. If a different column was given to the `y` argument that
  will be shown here.

- seas:

  Daily climatological cycle \[deg. C\].

- thresh:

  Daily varying threshold (e.g., 90th percentile) \[deg. C\]. This is
  used in
  [`detect_event3`](https://robwschlegel.github.io/heatwaveR/reference/detect_event3.md)
  for the detection/calculation of events (MHWs).

- var:

  Daily varying variance (standard deviation) \[deg. C\]. This column is
  not returned if `var = FALSE` (default).

Should `clmOnly` be enabled, only the 365 or 366 day climatology will be
returned.

## Details

1.  This function assumes that the input time series consists of
    continuous daily values with few missing values. Time ranges which
    start and end part-way through the calendar year are supported.

2.  It is recommended that a period of at least 30 years is specified in
    order to produce a climatology that smooths out any decadal thermal
    periodicities that may be present. When calculated over at least 30
    years of data, such a climatology is called a 'climatological
    normal.' It is further advised that full the start and end dates for
    the climatology period result in full years, e.g. "1982-01-01" to
    "2011-12-31" or "1982-07-01" to "2012-06-30"; if not, this may
    result in an unequal weighting of data belonging with certain months
    within a time series. A daily climatology will be created; that is,
    the climatology will be comprised of one mean temperature for each
    day of the year (365 or 366 days, depending on how leap years are
    dealt with), and the mean will be based on a sample size that is a
    function of the length of time determined by the start and end
    values given to `climatologyPeriod` and the width of the sliding
    window specified in `windowHalfWidth`.

3.  This function supports leap years. This is done by ignoring Feb 29s
    for the initial calculation of the climatology and threshold. The
    values for Feb 29 are then linearly interpolated from the values for
    Feb 28 and Mar 1.

4.  Previous versions of
    [`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
    tested to see if some rows are duplicated, or if replicate
    temperature readings are present per day, but this has now been
    disabled. Should the user be concerned about such repeated
    measurements, we suggest that the necessary checks and fixes are
    implemented prior to feeding the time series to
    [`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).

The original Python algorithm was written by Eric Oliver, Institute for
Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
documented by Hobday et al. (2016).

## References

Hobday, A.J. et al. (2016). A hierarchical approach to defining marine
heatwaves, Progress in Oceanography, 141, pp. 227-238,
doi:10.1016/j.pocean.2015.12.014

## Author

Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver

## Examples

``` r
data.table::setDTthreads(threads = 1) # optimise for your code and local computer
res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res[1:10, ]
#>              t  temp    seas  thresh
#>         <Date> <num>   <num>   <num>
#>  1: 1982-01-01 20.94 21.6080 22.9605
#>  2: 1982-01-02 21.25 21.6348 22.9987
#>  3: 1982-01-03 21.38 21.6621 23.0376
#>  4: 1982-01-04 21.16 21.6895 23.0771
#>  5: 1982-01-05 21.26 21.7169 23.1130
#>  6: 1982-01-06 21.61 21.7436 23.1460
#>  7: 1982-01-07 21.74 21.7699 23.1775
#>  8: 1982-01-08 21.50 21.7958 23.2080
#>  9: 1982-01-09 21.40 21.8217 23.2366
#> 10: 1982-01-10 21.36 21.8478 23.2649

# Or if one only wants the 366 day climatology
res_clim <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                    clmOnly = TRUE)
res_clim[1:10, ]
#>       doy    seas  thresh
#>     <num>   <num>   <num>
#>  1:     1 21.6080 22.9605
#>  2:     2 21.6348 22.9987
#>  3:     3 21.6621 23.0376
#>  4:     4 21.6895 23.0771
#>  5:     5 21.7169 23.1130
#>  6:     6 21.7436 23.1460
#>  7:     7 21.7699 23.1775
#>  8:     8 21.7958 23.2080
#>  9:     9 21.8217 23.2366
#> 10:    10 21.8478 23.2649

# Or if one wants the variance column included in the results
res_var <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                   var = TRUE)
res_var[1:10, ]
#>              t  temp    seas  thresh    var
#>         <Date> <num>   <num>   <num>  <num>
#>  1: 1982-01-01 20.94 21.6080 22.9605 0.0772
#>  2: 1982-01-02 21.25 21.6348 22.9987 0.0779
#>  3: 1982-01-03 21.38 21.6621 23.0376 0.0763
#>  4: 1982-01-04 21.16 21.6895 23.0771 0.0780
#>  5: 1982-01-05 21.26 21.7169 23.1130 0.0817
#>  6: 1982-01-06 21.61 21.7436 23.1460 0.0869
#>  7: 1982-01-07 21.74 21.7699 23.1775 0.0914
#>  8: 1982-01-08 21.50 21.7958 23.2080 0.0947
#>  9: 1982-01-09 21.40 21.8217 23.2366 0.0977
#> 10: 1982-01-10 21.36 21.8478 23.2649 0.0989
```
