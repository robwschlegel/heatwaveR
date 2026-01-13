# Constructs a continuous, uninterrupted time series of temperatures (faster).

Takes a series of dates and temperatures, and if irregular (but
ordered), inserts missing dates and fills corresponding temperatures
with NAs.

## Usage

``` r
make_whole_fast(data)
```

## Arguments

- data:

  A data frame with columns for date (`ts_x`) and temperature (`ts_y`)
  data. Ordered daily data are expected, and although missing values
  (NA) can be accommodated, the function is only recommended when NAs
  occur infrequently, preferably at no more than three consecutive days.

## Value

The function will return a data frame with three columns. The column
headed `doy` (day-of-year) is the Julian day running from 1 to 366, but
modified so that the day-of-year series for non-leap-years runs 1...59
and then 61...366. For leap years the 60th day is February 29. The
`ts_x` column is a series of dates of class `Date`, while `y` is the
measured variable. This time series will be uninterrupted and continuous
daily values between the first and last dates of the input data.

## Details

1.  This function reads in daily data with the time vector specified as
    `Date` (e.g. "1982-01-01").

2.  It is up to the user to calculate daily data from sub-daily
    measurements. Leap years are automatically accommodated by this
    function.

3.  This function can handle some missing days, but this is not a
    licence to actually use these data for the detection of anomalous
    thermal events. Hobday et al. (2016) recommend gaps of no more than
    3 days, which may be adjusted by setting the `maxPadLength` argument
    of the
    [`ts2clm`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm.md)
    function. The longer and more frequent the gaps become the lower the
    fidelity of the annual climatology and threshold that can be
    calculated, which will not only have repercussions for the accuracy
    at which the event metrics can be determined, but also for the
    number of events that can be detected.

    The day-of-year (doy) vector is created in `make_whole_fast` and
    inserts rows in cases when the original data set has missing rows
    for some dates. Should the user be concerned about the potential for
    repeated measurements or worry that the time series is unordered, we
    suggest that the necessary checks and fixes are implemented prior to
    feeding the time series to `ts2clim` via `make_whole_fast`. When
    using this fast algorithm, we assume that the user has done all the
    necessary work to ensure that the time vector is ordered and without
    repeated measurements beforehand.

## Author

Smit, A. J., Villeneuve, A., Schlegel, R. W.
