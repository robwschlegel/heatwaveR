# heatwaveR

**heatwaveR** provides fast, research-grade tools to detect, classify,
and visualise heatwaves and cold-spells in daily or hourly temperature
data.

Use it to build climatologies, identify events, summarise metrics, make
publication-ready figures.

**heatwaveR** is the modern, faster successor to
[**RmarineHeatWaves**](https://github.com/ajsmit/RmarineHeatWaves),
which in turn was the original R translation of
[marineHeatWvaes](https://github.com/ecjoliver/marineHeatWaves). This
package implements the Hobday et al. conventions that are used by many
marine heatwave researchers around the world.

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
devtools::install_github("robwschlegel/heatwaveR")
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

## Core functions

| Function | Role |
|----|----|
| [`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md) / [`ts2clm3()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm3.md) | Construct seasonal climatologies and thresholds from a time series. |
| [`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md) / [`detect_event3()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event3.md) | Detect heatwaves or cold-spells and return full event metrics. |
| [`category()`](https://robwschlegel.github.io/heatwaveR/reference/category.md) | Apply Hobday et al. (2018) event categories. |
| [`exceedance()`](https://robwschlegel.github.io/heatwaveR/reference/exceedance.md) | Detect consecutive exceedances relative to a static threshold. |
| [`block_average()`](https://robwschlegel.github.io/heatwaveR/reference/block_average.md) | Summarise event metrics at annual or block scale. |
| [`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md), [`lolli_plot()`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md), [`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md), [`geom_lolli()`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md) | Create publication-ready visualisations of detected events. |

The package also ships example SST records for historical events in the
Mediterranean, Northwest Atlantic, and Western Australia.

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

Robert W. Schlegel <robwschlegel@gmail.com>

## References

- Hobday, A. J., et al. (2016). A hierarchical approach to defining
  marine heatwaves. *Progress in Oceanography, 141*, 227-238.
- Hobday, A. J., Oliver, E. C. J., Sen Gupta, A., Benthuysen, J. A.,
  Burrows, M. T., Donat, M. G., Holbrook, N. J., Moore, P. J.,
  Thomsen, M. S., Wernberg, T., & Smale, D. A. (2018). Categorizing and
  naming marine heatwaves. *Oceanography, 31*(2).
