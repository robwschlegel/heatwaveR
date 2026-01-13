# Detect heatwaves and cold-spells.

`detect_event3` is a data.table version of the earlier
[`detect_event`](https://robwschlegel.github.io/heatwaveR/index.html/reference/detect_event.md).
It applies the Hobday et al. (2016) marine heat wave definition to an
input time series of a given value (usually, but not necessarily limited
to, temperature) along with a daily date vector and pre-calculated
seasonal and threshold climatologies, which may either be created with
[`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md)
or some other means.

## Usage

``` r
detect_event3(
  data,
  x = t,
  y = temp,
  seasClim = seas,
  threshClim = thresh,
  threshClim2 = NA,
  minDuration = 5,
  minDuration2 = minDuration,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxGap2 = maxGap,
  coldSpells = FALSE,
  protoEvents = FALSE,
  categories = FALSE,
  roundRes = 4,
  ...
)
```

## Arguments

- data:

  A data frame with at least four columns. In the default setting (i.e.
  omitting the arguments `x`, `y`, `seas`, and `thresh`; see immediately
  below), the data set is expected to have the headers `t`, `temp`,
  `seas`, and `thresh`. The `t` column is a vector of dates of class
  `Date`, `temp` is the measured variable (by default it is assumed to
  be temperature), `seas` is the seasonal cycle daily climatology (366
  days), and `thresh` is the seasonal cycle daily threshold above which
  events may be detected. Data of the appropriate format are created by
  the function
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md),
  but your own data can be supplied if they meet the criteria specified
  by
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md).
  If the column names of `data` match those outlined here, the following
  four arguments may be ignored.

- x:

  This column is expected to contain a vector of dates as per the
  specification of
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md).
  If a column headed `t` is present in the data.table, this argument may
  be omitted; otherwise, specify the name of the column with dates here.

- y:

  This is a column containing the measurement variable. If the column
  name differs from the default (i.e. `temp`), specify the name here.

- seasClim:

  The default for this argument assumes that the seasonal climatology
  column is called `seas` as this matches the output of
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md).
  If the column name for the seasonal climatology is different, provide
  that here.

- threshClim:

  The threshold climatology column should be called `thresh`. If it is
  not, provide the name of the threshold column here.

- threshClim2:

  If one wishes to provide a second climatology threshold filter for the
  more rigorous detection of events, a vector or column containing
  logical values (i.e. TRUE FALSE) should be provided here. By default
  this argument is ignored. It's primary purpose is to allow for the
  inclusion of tMin and tMax thresholds.

- minDuration:

  The minimum duration for acceptance of detected events. The default is
  `5` days.

- minDuration2:

  The minimum duration for acceptance of events after filtering by
  `threshClim` and `threshClim`. By default `minDuration2 = minDuration`
  and is ignored if `threshClim2` has not been specified.

- joinAcrossGaps:

  Boolean switch indicating whether to join events which occur
  before/after a short gap as specified by `maxGap`. The default is
  `TRUE`.

- maxGap:

  The maximum length of gap allowed for the joining of MHWs. The default
  is `2` days.

- maxGap2:

  The maximum gap length after applying both thresholds. By default
  `maxGap2 = maxGap` and is ignored if `threshClim2` has not been
  specified.

- coldSpells:

  Boolean specifying if the code should detect cold events instead of
  warm events. The default is `FALSE`. Please note that the
  climatological thresholds for cold-spells are considered to be the
  inverse of those for MHWs. For example, the default setting for the
  detection of MHWs is `pctile = 90`, as seen in
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md).
  Should one want to use `detect_event3` for MCSs, this threshold would
  best be generated in
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md)
  by setting `pctile = 10` (see example below). Any value may be used,
  but this is the setting used for the calculation of MCSs in Schlegel
  et al. (2017a).

- protoEvents:

  Boolean. With the default setting of `protoEvents = FALSE` a list with
  two components will be reported. The first component (`climatology`)
  will have the original time series returned by
  [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md)
  augmented with columns indicating if the threshold criterion
  (`threshCriterion`) and duration criterion (`durationCriterion`) have
  been exceeded, a column showing if a heatwave is present (i.e. both
  `threshCriterion` and `durationCriterion` are `TRUE`), and a
  sequential number uniquely identifying the detected event(s). The
  second list component (`event`) will contain the heatwave event
  metrics. If `protoEvents = TRUE` then only the `climatology` will be
  reported. Note also that if `protoEvents = TRUE` it will ignore
  whatever the user provides to the `categories` argument and anything
  else passed to `...`.

- categories:

  Boolean. Rather than using
  [`category`](https://robwschlegel.github.io/heatwaveR/index.html/reference/category.md)
  as a separate step to determine the categories of the detected MHWs,
  one may choose to set this argument to `TRUE`. One may pass the same
  arguments used in the
  [`category`](https://robwschlegel.github.io/heatwaveR/index.html/reference/category.md)
  function to this function to affect the output. Note that the default
  behaviour of
  [`category`](https://robwschlegel.github.io/heatwaveR/index.html/reference/category.md)
  is to return the event data only. To return the same list structure
  that `detect_event3` outputs by default, add the argument
  `climatology = TRUE`. By default `categories = FALSE`.

- roundRes:

  This argument allows the user to choose how many decimal places the
  MHW metric outputs will be rounded to. Default is 4. To prevent
  rounding set `roundRes = FALSE`. This argument may only be given
  numeric values or FALSE.

- ...:

  Allows unused arguments to pass through the functions.

## Value

The function will return a list of two data.frames, `climatology` and
`event`, which are, surprisingly, the climatology and event results,
respectively. The climatology contains the full time series of daily
temperatures, as well as the the seasonal climatology, the threshold and
various aspects of the events that were detected. The software was
designed for detecting extreme thermal events, and the units specified
below reflect that intended purpose. However, various other kinds of
extreme events may be detected according to the specifications, and if
that is the case, the appropriate units need to be determined by the
user.

Note that the exact content of the output depends on specific
combinations of the arguments `protoEvents`, `categories`, and
`climatology`:

- threshCriterion:

  Boolean indicating if `temp` exceeds `thresh`.

- durationCriterion:

  Boolean indicating whether periods of consecutive `threshCriterion`
  are \>= `min_duration`.

- event:

  Boolean indicating if all criteria that define an extreme event are
  met.

- event_no:

  A sequential number indicating the ID and order of occurrence of the
  events.

- intensity:

  The difference between `temp` (or the column provided for `y`) and
  `seas`. Only added if `categories = TRUE` and `climatology = TRUE`.

- category:

  The category classification per day. Only added if `categories = TRUE`
  and `climatology = TRUE`.

The `event` results are summarised using a range of event metrics:

- event_no:

  A sequential number indicating the ID and order of the events.

- index_start:

  Start index of event.

- index_end:

  End index of event.

- duration:

  Duration of event \[days\].

- date_start:

  Start date of event \[date\].

- date_end:

  End date of event \[date\].

- date_peak:

  Date of event peak \[date\].

- intensity_mean:

  Mean intensity \[deg. C\].

- intensity_max:

  Maximum (peak) intensity \[deg. C\].

- intensity_var:

  Intensity variability (standard deviation) \[deg. C\].

- intensity_cumulative:

  Cumulative intensity \[deg. C x days\].

- rate_onset:

  Onset rate of event \[deg. C / day\].

- rate_decline:

  Decline rate of event \[deg. C / day\].

`intensity_max_relThresh`, `intensity_mean_relThresh`,
`intensity_var_relThresh`, and `intensity_cumulative_relThresh` are as
above except relative to the threshold (e.g., 90th percentile) rather
than the seasonal climatology.

`intensity_max_abs`, `intensity_mean_abs`, `intensity_var_abs`, and
`intensity_cumulative_abs` are as above except as absolute magnitudes
rather than relative to the seasonal climatology or threshold.

Note that `rate_onset` and `rate_decline` will return `NA` when the
event begins/ends on the first/last day of the time series. This may be
particularly evident when the function is applied to large gridded data
sets. Although the other metrics do not contain any errors and provide
sensible values, please take this into account in its interpretation.

## Details

1.  This function assumes that the input time series consists of
    continuous daily values with few missing values. Time ranges which
    start and end part-way through the calendar year are supported. The
    accompanying function
    [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md)
    aids in the preparation of a time series that is suitable for use
    with `detect_event3`, although this may also be accomplished 'by
    hand' as long as the criteria are met as discussed in the
    documentation to
    [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md).

2.  The calculation of onset and decline rates assumes that the events
    started a half-day before the start day and ended a half-day after
    the end-day. This is consistent with the duration definition as
    implemented, which assumes duration = end day - start day + 1. An
    event that is already present at the beginning of a time series, or
    an event that is still present at the end of a time series, will
    report the rate of onset or the rate of decline as `NA`, as it is
    impossible to know what the temperature half a day before or after
    the start or end of the event is.

3.  For the purposes of event detection, any missing temperature values
    not interpolated over (through optional `maxPadLength` in
    [`ts2clm3`](https://robwschlegel.github.io/heatwaveR/index.html/reference/ts2clm3.md))
    will be set equal to the seasonal climatology. This means they will
    trigger the end/start of any adjacent temperature values which
    satisfy the event definition criteria.

4.  If the code is used to detect cold events (`coldSpells = TRUE`),
    then it works just as for heat waves except that events are detected
    as deviations below the (100 - pctile)th percentile (e.g., the 10th
    instead of 90th) for at least 5 days. Intensities are reported as
    negative values and represent the temperature anomaly below
    climatology.

The original Python algorithm was written by Eric Oliver, Institute for
Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
documented by Hobday et al. (2016). The marine cold spell option was
implemented in version 0.13 (21 Nov 2015) of the Python module as a
result of our preparation of Schlegel et al. (2017), wherein the cold
events receive a brief overview.

## References

Hobday, A.J. et al. (2016). A hierarchical approach to defining marine
heatwaves, Progress in Oceanography, 141, pp. 227-238,
doi:10.1016/j.pocean.2015.12.014

Schlegel, R. W., Oliver, C. J., Wernberg, T. W., Smit, A. J. (2017).
Nearshore and offshore co-occurrences of marine heatwaves and
cold-spells. Progress in Oceanography, 151, pp. 189-205,
doi:10.1016/j.pocean.2017.01.004

## Author

Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver

## Examples

``` r
data.table::setDTthreads(threads = 1) # optimise for your code and local computer
res_clim <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
out <- detect_event3(res_clim)
# show a portion of the climatology:
out$climatology[1:10, ]
#>           ts_x  ts_y ts_seas ts_thresh threshCriterion durationCriterion  event
#>         <Date> <num>   <num>     <num>          <lgcl>            <lgcl> <lgcl>
#>  1: 1982-01-01 20.94 21.6080   22.9605           FALSE             FALSE  FALSE
#>  2: 1982-01-02 21.25 21.6348   22.9987           FALSE             FALSE  FALSE
#>  3: 1982-01-03 21.38 21.6621   23.0376           FALSE             FALSE  FALSE
#>  4: 1982-01-04 21.16 21.6895   23.0771           FALSE             FALSE  FALSE
#>  5: 1982-01-05 21.26 21.7169   23.1130           FALSE             FALSE  FALSE
#>  6: 1982-01-06 21.61 21.7436   23.1460           FALSE             FALSE  FALSE
#>  7: 1982-01-07 21.74 21.7699   23.1775           FALSE             FALSE  FALSE
#>  8: 1982-01-08 21.50 21.7958   23.2080           FALSE             FALSE  FALSE
#>  9: 1982-01-09 21.40 21.8217   23.2366           FALSE             FALSE  FALSE
#> 10: 1982-01-10 21.36 21.8478   23.2649           FALSE             FALSE  FALSE
#>     event_no
#>        <int>
#>  1:       NA
#>  2:       NA
#>  3:       NA
#>  4:       NA
#>  5:       NA
#>  6:       NA
#>  7:       NA
#>  8:       NA
#>  9:       NA
#> 10:       NA
# show some of the heat waves:
out$event[1:5, 1:10]
#>    event_no index_start index_peak index_end duration date_start  date_peak
#>       <int>       <int>      <int>     <int>    <num>     <Date>     <Date>
#> 1:        1         885        887       889        5 1984-06-03 1984-06-05
#> 2:        2         899        901       904        6 1984-06-17 1984-06-19
#> 3:        3         908        922       926       19 1984-06-26 1984-07-10
#> 4:        4        1023       1027      1029        7 1984-10-19 1984-10-23
#> 5:        5        1033       1034      1037        5 1984-10-29 1984-10-30
#>      date_end intensity_mean intensity_max
#>        <Date>          <num>         <num>
#> 1: 1984-06-07         1.7042        1.9218
#> 2: 1984-06-22         1.9851        2.1063
#> 3: 1984-07-14         1.9094        2.2165
#> 4: 1984-10-25         1.4626        1.8304
#> 5: 1984-11-02         1.3034        1.4887

# Or if one wants to calculate MCSs
res_clim <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
                    pctile = 10)
out <- detect_event3(res_clim, coldSpells = TRUE)
# show a portion of the climatology:
out$climatology[1:10, ]
#>           ts_x   ts_y  ts_seas ts_thresh threshCriterion durationCriterion
#>         <Date>  <num>    <num>     <num>          <lgcl>            <lgcl>
#>  1: 1982-01-01 -20.94 -21.6080  -20.5471           FALSE             FALSE
#>  2: 1982-01-02 -21.25 -21.6348  -20.5713           FALSE             FALSE
#>  3: 1982-01-03 -21.38 -21.6621  -20.5961           FALSE             FALSE
#>  4: 1982-01-04 -21.16 -21.6895  -20.6218           FALSE             FALSE
#>  5: 1982-01-05 -21.26 -21.7169  -20.6499           FALSE             FALSE
#>  6: 1982-01-06 -21.61 -21.7436  -20.6776           FALSE             FALSE
#>  7: 1982-01-07 -21.74 -21.7699  -20.7053           FALSE             FALSE
#>  8: 1982-01-08 -21.50 -21.7958  -20.7362           FALSE             FALSE
#>  9: 1982-01-09 -21.40 -21.8217  -20.7655           FALSE             FALSE
#> 10: 1982-01-10 -21.36 -21.8478  -20.7927           FALSE             FALSE
#>      event event_no
#>     <lgcl>    <int>
#>  1:  FALSE       NA
#>  2:  FALSE       NA
#>  3:  FALSE       NA
#>  4:  FALSE       NA
#>  5:  FALSE       NA
#>  6:  FALSE       NA
#>  7:  FALSE       NA
#>  8:  FALSE       NA
#>  9:  FALSE       NA
#> 10:  FALSE       NA
# show some of the cold-spells:
out$event[1:5, 1:10]
#>    event_no index_start index_peak index_end duration date_start  date_peak
#>       <int>       <int>      <int>     <int>    <num>     <Date>     <Date>
#> 1:        1          83         83        87        5 1982-03-24 1982-03-24
#> 2:        2         194        199       207       14 1982-07-13 1982-07-18
#> 3:        3         270        272       277        8 1982-09-27 1982-09-29
#> 4:        4         296        303       306       11 1982-10-23 1982-10-30
#> 5:        5         672        681       685       14 1983-11-03 1983-11-12
#>      date_end intensity_mean intensity_max
#>        <Date>          <num>         <num>
#> 1: 1982-03-28        -1.4543       -1.5877
#> 2: 1982-07-26        -1.8033       -2.7304
#> 3: 1982-10-04        -1.2378       -1.3891
#> 4: 1982-11-02        -1.0167       -1.2813
#> 5: 1983-11-16        -1.1806       -1.4954

# It is also possible to give two separate sets of threshold criteria

# To use a second static threshold we first use the exceedance function
thresh_19 <- exceedance(sst_Med, threshold = 19, minDuration = 10, maxGap = 0)$threshold
# Then we use that output when detecting our events
events_19 <- detect_event3(ts2clm3(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")),
                           threshClim2 = thresh_19$exceedance, minDuration2 = 10, maxGap2 = 0)

# If we want to use two different percentile thresholds
thresh_95 <- detect_event3(ts2clm3(sst_Med, pctile = 95,
                                   climatologyPeriod = c("1982-01-01", "2011-12-31")),
                           minDuration = 2, maxGap = 0)$climatology
# Then we use that output when detecting our events
events_95 <- detect_event3(ts2clm3(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")),
                           threshClim2 = thresh_95$event, minDuration2 = 2, maxGap2 = 0)
```
