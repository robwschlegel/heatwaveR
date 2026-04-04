# Calculate daily mean, threshold, and variance climatologies.

An internal function that helps to create the climatologies that are
then output with
[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).

## Usage

``` r
clim_calc(data, windowHalfWidth, pctile)
```

## Arguments

- data:

  The data given to this function during the calculations performed by
  [`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).

- windowHalfWidth:

  The width of the smoothing window to be applied. This width is doubled
  and centred around the point that the smoothing occurs. Default = 5,
  which makes an overall window size of 11.

- pctile:

  Threshold percentile (%) for detection of events (MHWs). Default is
  `90`th percentile.

## Value

The function returns the calculated climatologies.

## Author

Smit, A. J.
