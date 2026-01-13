# Pad NA gaps of user-defined width with interpolated values.

An internal function that helps to create a time series that will then
be used by
[`clim_calc`](https://robwschlegel.github.io/heatwaveR/index.html/reference/clim_calc.md)
within
[`ts2clm`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm.md)
to produce a climatology as desired by the user.

## Usage

``` r
na_interp(doy = doy, x = ts_x, y = ts_y, maxPadLength)
```

## Arguments

- doy:

  Date-of-year as per `make_whole_fast`.

- x:

  Date as per `make_whole_fast`.

- y:

  Measurement variable as per `make_whole_fast`.

- maxPadLength:

  Specifies the maximum length of days over which to interpolate (pad)
  missing data (specified as `NA`) in the input temperature time series;
  i.e., any consecutive blocks of NAs with length greater than
  `maxPadLength` will be left as `NA`. Set as an integer.

## Value

The function returns the data (a data.table) in a long format.

## Author

Smit, A. J.
