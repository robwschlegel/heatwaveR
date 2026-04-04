# Create a timeline of selected event metrics as 'lollipops'.

Visualise a timeline of several possible event metrics as 'lollipop'
graphs.

## Usage

``` r
lolli_plot(data, xaxis = date_peak, metric = intensity_max, event_count = 3)
```

## Arguments

- data:

  Output from the
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
  function.

- xaxis:

  The name of a column from the `event` data.frame in the output of
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md).
  Suggested choices are, but not limited to, of `event_no`, `date_start`
  or `date_peak`. Default is `date_peak`.

- metric:

  The name of a column from the `event` data.frame in the output of
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md).Suggested
  choices are, but not limited to, `intensity_mean`, `intensity_max`,
  `intensity_cumulative` and `duration`. Default is `intensity_max`.

- event_count:

  The number of top events to highlight, as determined by the column
  given to `metric`. Default is 3.

## Value

The function will return a graph of the intensity of the selected
`metric` along the y-axis and the chosen `xaxis` value. The number of
top events as per `event_count` will be highlighted in a brighter
colour. This function differs in use from
[`geom_lolli`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)
in that it creates a stand-alone figure. The benefit of this being that
one does not need any prior knowledge of `ggplot2` to create the figure.

## Author

Albertus J. Smit and Robert W. Schlegel

## Examples

``` r
ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res <- detect_event(ts)

library(ggplot2)

# The default output
lolli_plot(res)

```
