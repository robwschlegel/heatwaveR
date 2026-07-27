# heatwaveR

**heatwaveR** provides fast, research-grade tools to detect, classify,
and visualise heatwaves and cold-spells in daily or hourly temperature
data.

Use it to build climatologies, identify events, summarise metrics, make
publication-ready figures.

**heatwaveR** is the modern, faster successor to
[**RmarineHeatWaves**](https://github.com/ajsmit/RmarineHeatWaves),
which in turn was the original R translation of
[**marineHeatWaves**](https://github.com/ecjoliver/marineHeatWaves).
This package implements the Hobday et al. conventions that are used by
many marine heatwave researchers around the world.

[![CRAN version
badge](https://www.r-pkg.org/badges/version/heatwaveR)](https://cran.r-project.org/package=heatwaveR)
[![R CMD check
badge](https://github.com/robwschlegel/heatwaveR/workflows/R-CMD-check/badge.svg)](https://github.com/robwschlegel/heatwaveR/actions)
[![Codecov
badge](https://codecov.io/gh/robwschlegel/heatwaveR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/robwschlegel/heatwaveR?branch=master)
[![JOSS
badge](https://joss.theoj.org/papers/10.21105/joss.00821/status.svg)](https://doi.org/10.21105/joss.00821)

## Install

Install the CRAN release:

``` r

install.packages("heatwaveR")
```

Install the development version:

``` r

remotes::install_github("robwschlegel/heatwaveR")
```

## Start here

### Detect events

Build seasonal climatologies and identify heatwaves or cold-spells with
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
and
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md).

[Basic detection and
visualisation](https://robwschlegel.github.io/heatwaveR/articles/detection_and_visualisation.html)

### Classify severity

Add Hobday et al. event categories, summarise metrics, and compare
events across years or locations.

[Event category
workflow](https://robwschlegel.github.io/heatwaveR/articles/event_categories.html)

### Scale to gridded data

Prepare OISST or other gridded inputs, then move into larger workflows
with **heatwave3**.

[Gridded event
detection](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html)

## Where to go next

- Browse the [reference
  index](https://robwschlegel.github.io/heatwaveR/reference/index.html)
  for function-level details.
- Work through the [articles
  index](https://robwschlegel.github.io/heatwaveR/articles/index.html)
  for practical workflows.
- Explore the [interactive
  demo](https://robert-schlegel.shinyapps.io/demoMHW/) for a quick
  hands-on overview.
- See the [Marine Heatwave
  Tracker](https://www.marineheatwaves.org/tracker.html) for a live
  global application.
- Check the [citations
  page](https://robwschlegel.github.io/heatwaveR/CITATIONS.html) for
  published uses of the package.

## Citation

If you use **`heatwaveR`** in research or reporting, please cite:

Schlegel, R. W., & Smit, A. J. (2018). *heatwaveR: A central algorithm
for the detection of heatwaves and cold-spells*. Journal of Open Source
Software, 3(27), 821. <https://doi.org/10.21105/joss.00821>

In R:

``` r

citation("heatwaveR")
```

## Core functions

| Function | Role |
|----|----|
| [`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md) / [`ts2clm3()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm3.md) | Construct seasonal climatologies and thresholds from a time series. |
| [`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md) / [`detect_event3()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event3.md) | Detect heatwaves or cold-spells and return full event metrics. |
| [`category()`](https://robwschlegel.github.io/heatwaveR/reference/category.md) | Apply Hobday et al. (2018) event categories. |
| [`exceedance()`](https://robwschlegel.github.io/heatwaveR/reference/exceedance.md) | Detect consecutive exceedances relative to a static threshold. |
| [`block_average()`](https://robwschlegel.github.io/heatwaveR/reference/block_average.md) | Summarise event metrics at annual or block scale. |
| [`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md), [`lolli_plot()`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md), [`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md), [`geom_lolli()`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md) | Create publication-ready visualisations of detected events. |

## Output and metrics

The
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
function will return a list of two tibbles (see the **`tidyverse`**),
`climatology` and `event`, which are the time series climatology and MHW
(or MCS) events, respectively. The climatology contains the full time
series of daily temperatures, as well as the the seasonal climatology,
the threshold and various aspects of the events that were detected. The
software was designed for detecting extreme thermal events, and the
units specified below reflect that intended purpose. However, various
other kinds of extreme events (e.g. rainfall) may be detected according
to the ‘heatwave’ specifications, and if that is the case, the
appropriate `minDuration` etc. and units of measurement need to be
determined by the user.

| Climatology metric | Description |
|----|----|
| `doy` | Julian day (day-of-year). For non-leap years it runs 1…59 and 61…366, while leap years run 1…366. This column will be named differently if another name was specified to the `doy` argument. |
| `t` | The date of the temperature measurement. This column will be named differently if another name was specified to the `x` argument. |
| `temp` | If the software was used for the purpose for which it was designed, seawater temperature (deg. C) on the specified date will be returned. This column will of course be named differently if another kind of measurement was specified to the `y` argument. |
| `seas` | Climatological seasonal cycle (deg. C). |
| `thresh` | Seasonally varying threshold (e.g., 90th percentile) (deg. C). |
| `var` | Variance (standard deviation) per `doy` of `temp` (deg. C). (not returned by default as of v0.3.5) |
| `threshCriterion` | Boolean indicating if `temp` exceeds `thresh`. |
| `durationCriterion` | Boolean indicating whether periods of consecutive `threshCriterion` are \>= `minDuration`. |
| `event` | Boolean indicating if all criteria that define a MHW or MCS are met. |
| `event_no` | A sequential number indicating the ID and order of occurrence of the MHWs or MCSs. |

The events are summarised using a range of event metrics:

| Event metric | Description |
|----|----|
| `event_no` | A sequential number indicating the ID and order of the events. This allows one to match/join results between the `climatology` and `event` outputs. |
| `index_start` | Row number from the given time series where the event starts. |
| `index_peak` | Row number from the given time series where the event peaks. |
| `index_end` | Row number from the given time series where the event ends. |
| `duration` | Duration of event (days). |
| `date_start` | Start date of event (date). |
| `date_peak` | Date of event peak (date). |
| `date_end` | End date of event (date). |
| `intensity_mean` | Mean intensity (deg. C). |
| `intensity_max` | Maximum (peak) intensity (deg. C). |
| `intensity_var` | Intensity variability (standard deviation) (deg. C). |
| `intensity_cumulative` | Cumulative intensity (deg. C x days). |
| `rate_onset` | Onset rate of event (deg. C / day). |
| `rate_decline` | Decline rate of event (deg. C / day). |

`intensity_max_relThresh`, `intensity_mean_relThresh`,
`intensity_var_relThresh`, and `intensity_cumulative_relThresh` are as
above except relative to the threshold (e.g., 90th percentile) rather
than the seasonal climatology.

`intensity_max_abs`, `intensity_mean_abs`, `intensity_var_abs`, and
`intensity_cumulative_abs` are as above except as absolute magnitudes
rather than relative to the seasonal climatology or threshold.

Note that `rate_onset` and `rate_decline` will return `NA` when the
event begins/ends on the first/last day of the time series. This may be
particularly evident when the function is applied to large gridded
datasets. Although the other metrics do not contain any errors and
provide sensible values, please take this into account in the
interpretation of the output. It must also be noted that events whose
`date_peak` occur on the same day as the `date_start` or `date_end` of
the event will return small negative values. This tends to only occur in
areas with persistent ice cover.

## Technical note

This package does what
[RmarineHeatWaves](https://github.com/ajsmit/RmarineHeatWaves) does, but
faster. The entire package has been deconstructed, modularised, and we
are continuing to implement slow portions of the code in C++. This has
alleviated the bottlenecks that slowed down the climatology creation
portions of the code as well as generally creating an overall increase
in the speed of the calculations. Currently the R code runs about as
fast as the original python functions, at least in as far as applying it
to single time series of temperatures.

Readers familiar with both languages will know about the ongoing debate
around the relative speed of the two. In our experience, R can be as
fast as python, provided that attention is paid to finding ways to
reduce the computational inefficiencies that stem from i) the liberal
use of complex and inefficient non-atomic data structures, such as data
frames; ii) the reliance on non-vectorised calculations such as loops;
and iii) lazy (but convenient) coding that comes from drawing too
heavily on the **`tidyverse`** suite of packages. We will continue to
ensure that heatwaveR becomes more-and-more efficient so that it can be
applied to large gridded data products with ease. To that end, the
extension package **`heatwave3`** has been developed. This helps the
user to apply the code from **`heatwaveR`** directly onto their NetCDF
and other 3D gridded data files.

**`heatwaveR`** was also developed and released in order to better
accommodate the inclusion of the definitions of atmospheric heatwaves.
Additionally, **`heatwaveR`** also provides the first implementation of
a definition for a ‘compound heatwave’. There are currently multiple
different definitions for this type of event and each of which has
arguments provided for it within the
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
and
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
functions.

## Contributing and support

- Report bugs or request features on [GitHub
  issues](https://github.com/robwschlegel/heatwaveR/issues).
- See the [contribution
  guide](https://robwschlegel.github.io/heatwaveR/CONTRIBUTING.html) for
  development workflow details.
- Use the [news
  page](https://robwschlegel.github.io/heatwaveR/news/index.html) to
  review recent package changes.

## Acknowledgements

The [Python code](https://github.com/ecjoliver/marineHeatWaves) was
written by Eric C. J. Oliver.

Contributors to the Marine Heatwaves definition and its numerical
implementation include Alistair J. Hobday, Lisa V. Alexander, Sarah E.
Perkins, Dan A. Smale, Sandra C. Straub, Jessica Benthuysen, Michael T.
Burrows, Markus G. Donat, Ming Feng, Neil J. Holbrook, Pippa J. Moore,
Hillary A. Scannell, Alex Sen Gupta, and Thomas Wernberg.

The original translation from Python to R was done by A. J. Smit and the
graphing functions were contributed by Robert W. Schlegel.

## Contact

Robert W. Schlegel : <robwschlegel@gmail.com>

## References

- Hobday, A. J., et al. (2016). A hierarchical approach to defining
  marine heatwaves. *Progress in Oceanography, 141*, 227-238.
- Hobday, A. J., Oliver, E. C. J., Sen Gupta, A., Benthuysen, J. A.,
  Burrows, M. T., Donat, M. G., Holbrook, N. J., Moore, P. J.,
  Thomsen, M. S., Wernberg, T., & Smale, D. A. (2018). Categorizing and
  naming marine heatwaves. *Oceanography, 31*(2).
