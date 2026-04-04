---
output:
  md_document:
    variant: gfm
    fig_width: 8
    fig_height: 3
---



# heatwaveR

<p><strong>heatwaveR</strong> provides fast, research-grade tools to detect, classify, and visualise heatwaves and cold-spells in daily or hourly temperature data.</p><p>Use it to build climatologies, identify events, summarise metrics, make publication-ready figures.</p><p><strong>heatwaveR</strong> is the modern, faster successor to <a href="https://github.com/ajsmit/RmarineHeatWaves"><strong>RmarineHeatWaves</strong></a>, which in turn was the original R translation of [marineHeatWvaes](https://github.com/ecjoliver/marineHeatWaves). This package implements the Hobday et al. conventions that are used by many marine heatwave researchers around the world.</p>

<div class="pkgdown-badges">
  <a href="https://cran.r-project.org/package=heatwaveR"><img src="https://www.r-pkg.org/badges/version/heatwaveR" alt="CRAN version badge" /></a>
  <a href="https://github.com/robwschlegel/heatwaveR/actions"><img src="https://github.com/robwschlegel/heatwaveR/workflows/R-CMD-check/badge.svg" alt="R CMD check badge" /></a>
  <a href="https://app.codecov.io/gh/robwschlegel/heatwaveR?branch=master"><img src="https://codecov.io/gh/robwschlegel/heatwaveR/branch/master/graph/badge.svg" alt="Codecov badge" /></a>
  <a href="https://doi.org/10.21105/joss.00821"><img src="https://joss.theoj.org/papers/10.21105/joss.00821/status.svg" alt="JOSS badge" /></a>
</div>

## Install

Install the CRAN release:

```r
install.packages("heatwaveR")
```

Install the development version:

```r
remotes::install_github("robwschlegel/heatwaveR")
```

## Start here

<div class="hw-grid"><div class="hw-card"><h3>Detect events</h3><p>Build seasonal climatologies and identify heatwaves or cold-spells with <code>ts2clm()</code> and <code>detect_event()</code>.</p><p><a href="https://robwschlegel.github.io/heatwaveR/articles/detection_and_visualisation.html">Basic detection and visualisation</a></p></div><div class="hw-card"><h3>Classify severity</h3><p>Add Hobday et al. event categories, summarise metrics, and compare events across years or locations.</p><p><a href="https://robwschlegel.github.io/heatwaveR/articles/event_categories.html">Event category workflow</a></p></div><div class="hw-card"><h3>Scale to gridded data</h3><p>Prepare OISST or other gridded inputs, then move into larger workflows with <strong>heatwave3</strong>.</p><p><a href="https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html">Gridded event detection</a></p></div></div>

## Core functions

| Function | Role |
|----|----|
| `ts2clm()` / `ts2clm3()` | Construct seasonal climatologies and thresholds from a time series. |
| `detect_event()` / `detect_event3()` | Detect heatwaves or cold-spells and return full event metrics. |
| `category()` | Apply Hobday et al. (2018) event categories. |
| `exceedance()` | Detect consecutive exceedances relative to a static threshold. |
| `block_average()` | Summarise event metrics at annual or block scale. |
| `event_line()`, `lolli_plot()`, `geom_flame()`, `geom_lolli()` | Create publication-ready visualisations of detected events. |

## Where to go next

- Browse the [reference index](https://robwschlegel.github.io/heatwaveR/reference/index.html) for function-level details.
- Work through the [articles index](https://robwschlegel.github.io/heatwaveR/articles/index.html) for practical workflows.
- Explore the [interactive demo](https://robert-schlegel.shinyapps.io/demoMHW/) for a quick hands-on overview.
- See the [Marine Heatwave Tracker](https://www.marineheatwaves.org/tracker.html) for a live global application.
- Check the [citations page](https://robwschlegel.github.io/heatwaveR/CITATIONS.html) for published uses of the package.

## Citation

If you use **`heatwaveR`** in research or reporting, please cite:

Schlegel, R. W., & Smit, A. J. (2018). *heatwaveR: A central algorithm for the detection of heatwaves and cold-spells*. Journal of Open Source Software, 3(27), 821. https://doi.org/10.21105/joss.00821

In R:

```r
citation("heatwaveR")
```

## Contributing and support

- Report bugs or request features on [GitHub issues](https://github.com/robwschlegel/heatwaveR/issues).
- See the [contribution guide](https://robwschlegel.github.io/heatwaveR/CONTRIBUTING.html) for development workflow details.
- Use the [news page](https://robwschlegel.github.io/heatwaveR/news/index.html) to review recent package changes.

## Acknowledgements

The [Python code](https://github.com/ecjoliver/marineHeatWaves) was written by Eric C. J. Oliver.

Contributors to the Marine Heatwaves definition and its numerical implementation include Alistair J. Hobday, Lisa V. Alexander, Sarah E. Perkins, Dan A. Smale, Sandra C. Straub, Jessica Benthuysen, Michael T. Burrows, Markus G. Donat, Ming Feng, Neil J. Holbrook, Pippa J. Moore, Hillary A. Scannell, Alex Sen Gupta, and Thomas Wernberg.

The original translation from Python to R was done by A. J. Smit and the graphing functions were contributed by Robert W. Schlegel.

## Contact

Robert W. Schlegel
<robwschlegel@gmail.com>

## References

- Hobday, A. J., et al. (2016). A hierarchical approach to defining marine heatwaves. *Progress in Oceanography, 141*, 227-238.
- Hobday, A. J., Oliver, E. C. J., Sen Gupta, A., Benthuysen, J. A., Burrows, M. T., Donat, M. G., Holbrook, N. J., Moore, P. J., Thomsen, M. S., Wernberg, T., & Smale, D. A. (2018). Categorizing and naming marine heatwaves. *Oceanography, 31*(2).
