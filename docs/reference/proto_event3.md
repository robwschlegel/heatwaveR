# Detect proto-events based on a chosen criterion (column).

An internal function that detects the events according to the heatwave
definition, and joins across the gaps if desired. This is an update of
[`detect_event`](https://robwschlegel.github.io/heatwaveR/index.html/reference/detect_event.md)
and is made entirely within data.table to benefit from the speed
improvements.

## Usage

``` r
proto_event3(p_series, criterion_column, minDuration, joinAcrossGaps, maxGap)
```

## Arguments

- p_series:

  A data.table of the correct dimensions received from
  [`detect_event3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/detect_event3.md)
  within which this runs.

- criterion_column:

  The column to use for the detection of events.

- minDuration:

  Minimum duration for acceptance of detected events.

- joinAcrossGaps:

  This logic gate tells this internal function if it should connect
  events across the `maxGap` (see below). The default it inherits is
  `TRUE`.

- maxGap:

  This is the number of rows (days) across which distinct events will be
  combined into one event if `joinAcrossGaps = TRUE`.

## Value

A dataframe that will be used within
[`detect_event`](https://robwschlegel.github.io/heatwaveR/index.html/reference/detect_event.md),
or which can be returned by
[`detect_event`](https://robwschlegel.github.io/heatwaveR/index.html/reference/detect_event.md)
if the switch 'protoEvent' is specified as TRUE.

## Author

Albertus J. Smit, Robert W. Schlegel
