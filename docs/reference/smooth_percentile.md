# Detect the climatology for a time series.

An internal function that helps to create climatologies in accordance
with the Hobday et al. (2016) standard.

## Usage

``` r
smooth_percentile(data, smoothPercentileWidth, var_calc)
```

## Arguments

- data:

  The data given to this function during the calculations performed by
  [`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).

- smoothPercentileWidth:

  The width of the smoothing window to be applied. The default is `31`
  days.

- var_calc:

  This is passed from the ts2clm argument `var` and tells the function
  if a var column exists that needs to be smoothed.

## Value

The function returns the data in the same format it was input as, with
the climatology values smoothed as desired.

## Author

Smit, A. J., Schlegel, R. W.
