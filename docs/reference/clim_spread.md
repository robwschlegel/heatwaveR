# Spread a time series wide to allow for a climatology to be calculated.

An internal function that helps to create a wide time series that will
then be used by
[`clim_calc`](https://robwschlegel.github.io/heatwaveR/reference/clim_calc.md)
within
[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
to produce a climatology as desired by the user.

## Usage

``` r
clim_spread(data, clim_start, clim_end, windowHalfWidth)
```

## Arguments

- data:

  The data given to this function during the calculations performed by
  [`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).

- clim_start:

  The first day of the time series to use when spreading.

- clim_end:

  The last day of the time series to use when spreading.

- windowHalfWidth:

  The width of the smoothing window to be applied. This width is doubled
  and centred around the point that the smoothing occurs. Default = 5,
  which makes an overall window size of 11.

## Value

The function returns the data (a matrix) in a wide format.

## Author

Smit, A. J., Villeneuve, A., Schlegel, R. W.
