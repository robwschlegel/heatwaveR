heatwaveR <img src="vignettes/logo.png" width=200 align="right" />
==================================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/heatwaveR)](https://cran.r-project.org/package=heatwaveR) [![Travis build status](https://travis-ci.org/robwschlegel/heatwaveR.svg?branch=master)](https://travis-ci.org/robwschlegel/heatwaveR) [![Coverage status](https://codecov.io/gh/robwschlegel/heatwaveR/branch/master/graph/badge.svg)](https://codecov.io/github/robwschlegel/heatwaveR?branch=master) [![JOSS](http://joss.theoj.org/papers/10.21105/joss.00821/status.svg)](https://doi.org/10.21105/joss.00821) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1324309.svg)](https://doi.org/10.5281/zenodo.1324309) ![](https://cranlogs.r-pkg.org/badges/grand-total/heatwaveR)

The **`heatwaveR`** package is a project-wide update to the [**`RmarineHeatWaves`**](https://github.com/ajsmit/RmarineHeatWaves) package, which is itself a translation of the original [Python code](https://github.com/ecjoliver/marineHeatWaves) written by Eric C. J. Oliver. The **`heatwaveR`** package also uses the same naming conventions for objects, columns, and arguments as the Python code.

The **`heatwaveR`** R package contains the original functions from the **`RmarineHeatWaves`** package that calculate and display marine heatwaves (MHWs) according to the definition of Hobday et al. (2016) as well as calculating and visualising marine cold-spells (MCSs) as first introduced in Schlegel et al. (2017a). It also contains the functionality to calculate the categories of MHWs as outlined in Hobday et al. (2018).

This package does what **`RmarineHeatWaves`** does, but faster. The entire package has been deconstructed and modularised, and slow portions of the code are being implemented in C++. C++ has already replaced some of the bottlenecks that slowed down the climatology creation portions of the code, and we will slowly but surely improve the efficiency and speed in other portions of the code too. Currently the R code runs about as fast as the original python functions, at least in as far as applying it to single time series of temperatures. Readers familiar with both languages will know about the ongoing debate around the relative speed of the two languages. In our experience, R can be as fast as python, provided that attention is paid to finding ways to reduce the computational inefficiencies that stem from i) the liberal use of complex and inefficient non-atomic data structures, such as data frames; ii) the reliance on non-vectorised calculations such as loops; and iii) lazy (but convenient) coding that comes from drawing too heavily on the `tidyverse` suite of packages. We will continue to ensure that **`heatwaveR`** becomes more-and-more efficient so that it can be applied to large gridded data products with ease.

This new package was developed and released in order to better accommodate the inclusion of the definitions of atmospheric heatwaves in addition to MHWs. Additionally, **`heatwaveR`** also provides the first implementation of a definition for a ‘compound heatwave’. There are currently multiple different definitions for this type of event and each of which has arguments provided for it within the `ts2clm()` and `detect_event()` functions.

This package may be installed from CRAN by typing the following command into the console:

`install.packages("heatwaveR")`

Or the development version may be installed from GitHub with:

`devtools::install_github("robwschlegel/heatwaveR")`

The functions
-------------

<table>
<colgroup>
<col width="17%" />
<col width="82%" />
</colgroup>
<thead>
<tr class="header">
<th>Function</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>ts2clm()</code></td>
<td>Constructs seasonal and threshold climatologies as per the definition of Hobday et al. (2016).</td>
</tr>
<tr class="even">
<td><code>detect_event()</code></td>
<td>The main function which detects the events as per the definition of Hobday et al. (2016).</td>
</tr>
<tr class="odd">
<td><code>block_average()</code></td>
<td>Calculates annual means for event metrics.</td>
</tr>
<tr class="even">
<td><code>category()</code></td>
<td>Applies event categories to the output of <code>detect_event()</code> based on Hobday et al. (in review).</td>
</tr>
<tr class="odd">
<td><code>exceedance()</code></td>
<td>A function similar to <code>detect_event()</code> but that detects consecutive days above/below a given static threshold.</td>
</tr>
<tr class="even">
<td><code>event_line()</code></td>
<td>Creates a line plot of heatwaves or cold-spells.</td>
</tr>
<tr class="odd">
<td><code>lolli_plot()</code></td>
<td>Creates a timeline of selected event metrics.</td>
</tr>
<tr class="even">
<td><code>geom_flame()</code></td>
<td>Creates flame polygons of heatwaves or cold-spells.</td>
</tr>
<tr class="odd">
<td><code>geom_lolli()</code></td>
<td>Creates a lolliplot timeline of selected event metric.</td>
</tr>
</tbody>
</table>

The package also provides data of observed SST records for three historical MHWs: the 2011 Western Australia event, the 2012 Northwest Atlantic event and the 2003 Mediterranean event.

The heatwave metrics
--------------------

The function will return a list of two tibbles (see the ‘tidyverse’), `clim` and `event`, which are the climatology and MHW (or MCS) events, respectively. The climatology contains the full time series of daily temperatures, as well as the the seasonal climatology, the threshold and various aspects of the events that were detected. The software was designed for detecting extreme thermal events, and the units specified below reflect that intended purpose. However, the various other kinds of extreme events may be detected according to the ‘marine heatwave’ specifications, and if that is the case, the appropriate units need to be determined by the user.

<table>
<colgroup>
<col width="16%" />
<col width="83%" />
</colgroup>
<thead>
<tr class="header">
<th>Climatology metric</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>doy</code></td>
<td>Julian day (day-of-year). For non-leap years it runs 1…59 and 61…366, while leap years run 1…366. This column will be named differently if another name was specified to the <code>doy</code> argument.</td>
</tr>
<tr class="even">
<td><code>t</code></td>
<td>The date of the temperature measurement. This column will be named differently if another name was specified to the <code>x</code> argument.</td>
</tr>
<tr class="odd">
<td><code>temp</code></td>
<td>If the software was used for the purpose for which it was designed, seawater temperature [deg. C] on the specified date will be returned. This column will of course be named differently if another kind of measurement was specified to the <code>y</code> argument.</td>
</tr>
<tr class="even">
<td><code>seas</code></td>
<td>Climatological seasonal cycle [deg. C].</td>
</tr>
<tr class="odd">
<td><code>thresh</code></td>
<td>Seasonally varying threshold (e.g., 90th percentile) [deg. C].</td>
</tr>
<tr class="even">
<td><code>var</code></td>
<td>Seasonally varying variance (standard deviation) [deg. C].</td>
</tr>
<tr class="odd">
<td><code>threshCriterion</code></td>
<td>Boolean indicating if <code>temp</code> exceeds <code>thresh</code>.</td>
</tr>
<tr class="even">
<td><code>durationCriterion</code></td>
<td>Boolean indicating whether periods of consecutive <code>threshCriterion</code> are &gt;= <code>minDuration</code>.</td>
</tr>
<tr class="odd">
<td><code>event</code></td>
<td>Boolean indicating if all criteria that define a MHW or MCS are met.</td>
</tr>
<tr class="even">
<td><code>event_no</code></td>
<td>A sequential number indicating the ID and order of occurrence of the MHWs or MCSs.</td>
</tr>
</tbody>
</table>

The events are summarised using a range of event metrics:

| Event metric           | Description                                                    |
|------------------------|----------------------------------------------------------------|
| `event_no`             | A sequential number indicating the ID and order of the events. |
| `index_start`          | Start index of event.                                          |
| `index_peak`           | Peak index of event.                                           |
| `index_end`            | Index of event peak.                                           |
| `duration`             | Duration of event \[days\].                                    |
| `date_start`           | Start date of event \[date\].                                  |
| `date_peak`            | Date of event peak \[date\].                                   |
| `date_end`             | End date of event \[date\].                                    |
| `intensity_mean`       | Mean intensity \[deg. C\].                                     |
| `intensity_max`        | Maximum (peak) intensity \[deg. C\].                           |
| `intensity_var`        | Intensity variability (standard deviation) \[deg. C\].         |
| `intensity_cumulative` | Cumulative intensity \[deg. C x days\].                        |
| `rate_onset`           | Onset rate of event \[deg. C / day\].                          |
| `rate_decline`         | Decline rate of event \[deg. C / day\].                        |

`intensity_max_relThresh`, `intensity_mean_relThresh`, `intensity_var_relThresh`, and `intensity_cumulative_relThresh` are as above except relative to the threshold (e.g., 90th percentile) rather than the seasonal climatology.

`intensity_max_abs`, `intensity_mean_abs`, `intensity_var_abs`, and `intensity_cumulative_abs` are as above except as absolute magnitudes rather than relative to the seasonal climatology or threshold.

Note that `rate_onset` and `rate_decline` will return `NA` when the event begins/ends on the first/last day of the time series. This may be particularly evident when the function is applied to large gridded data sets. Although the other metrics do not contain any errors and provide sensible values, please take this into account in the interpretation of the output.

Vignettes
---------

For detailed explanations and walkthroughs on the use of the **`heatwaveR`** package please click on the articles tab above, or follow the links below:

-   For a basic introduction to the [detection and visualisation](https://robwschlegel.github.io/heatwaveR/articles/detection_and_visualisation.html) of events.
-   For an explanation on the use of the [exceedance](https://robwschlegel.github.io/heatwaveR/articles/exceedance.html) function.
-   For a walkthrough on the calculation and visualisation of [event categories](https://robwschlegel.github.io/heatwaveR/articles/event_categories.html).
-   For examples on the calculation of [complex climatologies](https://robwschlegel.github.io/heatwaveR/articles/complex_clims.html).
-   For a demonstration on how to [download and prepare OISST data](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html).
-   Which may then have the `detect_event()` function applied to the [gridded data](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html), and then fit a GLM and plot the results.

Contributing to **`heatwaveR`**
-------------------------------

To contribute to the package please follow the guidelines [here](https://robwschlegel.github.io/heatwaveR/CONTRIBUTING.html).

Please use this [link](https://github.com/robwschlegel/heatwaveR/issues) to report any bugs found.

References
----------

Hobday, A.J. et al. (2016). A hierarchical approach to defining marine heatwaves, Progress in Oceanography, 141, pp. 227-238.

Schlegel, R. W., Oliver, E. C. J., Wernberg, T. W., Smit, A. J. (2017a). Nearshore and offshore co-occurrences of marine heatwaves and cold-spells. Progress in Oceanography, 151, pp. 189-205.

Schlegel, R. W., Oliver, E. C., Perkins-Kirkpatrick, S., Kruger, A., Smit, A. J. (2017b). Predominant atmospheric and oceanic patterns during coastal marine heatwaves. Frontiers in Marine Science, 4, 323.

Hobday, A. J., Oliver, E. C. J., Sen Gupta, A., Benthuysen, J. A., Burrows, M. T., Donat, M. G., Holbrook, N. J., Moore, P. J., Thomsen, M. S., Wernberg, T., Smale, D. A. (2018). Categorizing and naming marine heatwaves. Oceanography 31(2).

Acknowledgements
----------------

The Python code was written by Eric C. J. Oliver.

Contributors to the Marine Heatwaves definition and its numerical implementation include Alistair J. Hobday, Lisa V. Alexander, Sarah E. Perkins, Dan A. Smale, Sandra C. Straub, Jessica Benthuysen, Michael T. Burrows, Markus G. Donat, Ming Feng, Neil J. Holbrook, Pippa J. Moore, Hillary A. Scannell, Alex Sen Gupta, and Thomas Wernberg.

The translation from Python to R was done by A. J. Smit and the graphing functions were contributed to by Robert. W. Schlegel.

Contact
-------

Robert W. Schlegel
Department for Biodiversity & Conservation Biology, University of the Western Cape
Private Bag X17, Bellville 7535, South Africa
E-mail: <robwschlegel@gmail.com> <!-- Work tel.: +27 (0)21 959 3783 -->
