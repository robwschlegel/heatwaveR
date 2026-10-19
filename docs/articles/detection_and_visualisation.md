# Basic Detection and Visualisation of Events

## Data

The
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
function is the core of this package, and it expects to be fed the
output of the second core function,
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md).
By default,
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
wants to receive a two-column dataframe with one column labelled `t`
containing all of the date values, and a second column `temp` containing
all of the temperature values. Please note that the date format it
expects is “YYYY-MM-DD”. For example, please see the top five rows of
one of the datasets included with the **`heatwaveR`** package:

\
[`head`](https://rdrr.io/r/utils/head.html)`(``heatwaveR``::`[`sst_WA`](https://robwschlegel.github.io/heatwaveR/reference/sst_WA.md)`)`

    ## # A tibble: 6 × 2
    ##   t           temp
    ##   <date>     <dbl>
    ## 1 1982-01-01  20.9
    ## 2 1982-01-02  21.2
    ## 3 1982-01-03  21.4
    ## 4 1982-01-04  21.2
    ## 5 1982-01-05  21.3
    ## 6 1982-01-06  21.6

It is possible to use different column names other than `t` and `temp`
with which to calculate events. Please see the help files for
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
or
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
for a thorough explanation of how to do so.

Loading ones data from a `.csv` file or other text based format is the
easiest approach for the calculation of events, assuming one is not
working with gridded data (e.g. NetCDF). Please see [this
vignette](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html)
for a detailed walkthrough on using the functions in this package with
gridded data.

## Calculating marine heatwaves (MHWs)

Here are the
[`ts2clm()`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)
and
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
function applied to the Western Australia test data included with this
package (`sst_WA`), which are also discussed by Hobday et al. (2016):

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`heatwaveR`](https://robwschlegel.github.io/heatwaveR/)`)`\
\
`# Detect the events in a time series`\
`ts`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_WA``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)`\
`mhw`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``ts``)`\
\
`# View just a few metrics`\
`mhw``$``event`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``event_no``, ``duration``, ``date_start``, ``date_peak``, ``intensity_max``, ``intensity_cumulative``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``-``intensity_max``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``5``)`

    ##   event_no duration date_start  date_peak intensity_max intensity_cumulative
    ## 1       52      105 2010-12-24 2011-02-28        6.5798             293.2107
    ## 2       41       35 2008-03-25 2008-04-14        3.8299              79.3307
    ## 3       29       95 1999-05-13 1999-05-22        3.6390             240.2994
    ## 4       60       14 2012-12-27 2012-12-31        3.4230              32.2560
    ## 5       59      101 2012-01-10 2012-01-27        3.3804             214.0509

## Visualising marine heatwaves (MHWs)

### Default MHW visuals

One may use
[`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)
and
[`lolli_plot()`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md)
directly on the output of
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
in order to visualise MHWs. Here are the functions being used to
visualise the massive Western Australian heatwave of 2011:

\
[`event_line`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)`(``mhw``, spread ``=`` ``180``, metric ``=`` ``intensity_max``,`\
`           start_date ``=`` ``"1982-01-01"``, end_date ``=`` ``"2014-12-31"``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.14``, ``0.13``)``,`\
`        legend.direction ``=`` ``"vertical"``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Line plot of Western Australia sea surface temperature from mid-2010
to late 2011, showing the climatological mean, the 90th-percentile
threshold, and the 2011 marine heatwave shaded in red with its peak
intensity
marked.](detection_and_visualisation_files/figure-html/fig-example1-1.png)

\
[`lolli_plot`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md)`(``mhw``, metric ``=`` ``intensity_max``)`

![Lollipop plot of all detected Western Australia marine heatwave events
by start date, with each event's maximum intensity shown as a stem and
the 2011 event highlighted as the most
extreme.](detection_and_visualisation_files/figure-html/fig-example1-2.png)

### Custom MHW visuals

The
[`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)
and
[`lolli_plot()`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md)
functions were designed to work directly on the list returned by
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md).
If more control over the figures is required, it may be useful to create
them in **`ggplot2`** by stacking `geoms`. We specifically created two
new **`ggplot2`** `geoms` to reproduce the functionality of
[`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)
and
[`lolli_plot()`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md).
These functions are more general in their functionality and can be used
outside of the **`heatwaveR`** package, too. To apply them to MHWs and
MCSs first requires that we access the `climatology` or `event`
dataframes within the list that is produced by
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md).
Here is how:

\
`# Select the region of the time series of interest`\
`mhw2`` ``<-`` ``mhw``$``climatology`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``10580``:``10720``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mhw2``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``temp``, y2 ``=`` ``thresh``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``)`` ``+`\
`  `[`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2011-02-25"``)``, y ``=`` ``25.8``, label ``=`` ``"the Destroyer\nof Kelps"``)``)`

![Custom ggplot2 geom_flame plot of temperature against the seasonal
threshold during the 2011 Western Australia marine heatwave, with the
exceedance area shaded as a flame shape and labelled 'the Destroyer of
Kelps'.](detection_and_visualisation_files/figure-html/fig-example2-1.png)

\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mhw``$``event``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``date_start``, y ``=`` ``intensity_max``)``)`` ``+`\
`  `[`geom_lolli`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)`(``colour ``=`` ``"salmon"``, colour_n ``=`` ``"red"``, n ``=`` ``3``)`` ``+`\
`  `[`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)`(``colour ``=`` ``"black"``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2006-08-01"``)``, y ``=`` ``5``,`\
`                label ``=`` ``"The marine heatwaves\nTend to be left skewed in a\nGiven time series"``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Max. intensity ["``, ``degree``, ``"C]"``)``)``, x ``=`` ``NULL``)`

![Custom ggplot2 geom_lolli plot of marine heatwave maximum intensity by
start date, with the three most intense events highlighted in red and a
label noting that marine heatwave intensities tend to be
left-skewed.](detection_and_visualisation_files/figure-html/fig-example2-2.png)

### Spicy MHW visuals

The default output of these function may not be to your liking. If so,
not to worry. As **`ggplot2`** `geoms`, they are highly malleable. For
example, if we were to choose to reproduce the format of the MHWs as
seen in Hobday et al. (2016), the code would look something like this:

\
`# It is necessary to give geom_flame() at least one row on either side of `\
`# the event in order to calculate the polygon corners smoothly`\
`mhw_top`` ``<-`` ``mhw2`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``5``:``111``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``mhw2``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, y2 ``=`` ``thresh``, fill ``=`` ``"all"``)``, show.legend ``=`` ``T``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``data ``=`` ``mhw_top``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, y2 ``=`` ``thresh``, fill ``=`` ``"top"``)``,  show.legend ``=`` ``T``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, colour ``=`` ``"temp"``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, colour ``=`` ``"thresh"``)``, size ``=`` ``1.0``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``seas``, colour ``=`` ``"seas"``)``, size ``=`` ``1.2``)`` ``+`\
`  `[`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``"Line Colour"``,`\
`                      values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"temp"`` ``=`` ``"black"``, `\
`                                 ``"thresh"`` ``=``  ``"forestgreen"``, `\
`                                 ``"seas"`` ``=`` ``"grey80"``)``)`` ``+`\
`  `[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``"Event Colour"``,`\
`                    values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"all"`` ``=`` ``"salmon"``,`\
`                               ``"top"`` ``=`` ``"red"``)``)`` ``+`\
`  `[`scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)`(``date_labels ``=`` ``"%b %Y"``)`` ``+`\
`  `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``override.aes ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``fill ``=`` ``NA``)``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Temperature ["``, ``degree``, ``"C]"``)``)``, x ``=`` ``NULL``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.12``, ``0.55``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Reproduction of the Hobday et al. (2016) style marine heatwave figure,
showing temperature, the seasonal climatology, and the 90th-percentile
threshold as lines, with the full heatwave event shaded in salmon and
its most intense portion shaded in red, each with a colour
legend.](detection_and_visualisation_files/figure-html/fig-example3-1.png)

It is also worth pointing out that when we use
[`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)
directly like this, but we don’t want to highlight events greater less
than our standard five day length, allowing for a two day gap, we want
to use the arguments `n` and `n_gap` respectively.

\
`mhw3`` ``<-`` ``mhw``$``climatology`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``850``:``950``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mhw3``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``temp``, y2 ``=`` ``thresh``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``fill ``=`` ``"black"``, alpha ``=`` ``0.5``)`` ``+`\
`  ``# Note the use of n = 5 and n_gap = 2 below`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``n ``=`` ``5``, n_gap ``=`` ``2``, fill ``=`` ``"red"``, alpha ``=`` ``0.5``)`` ``+`\
`  `[`ylim`](https://ggplot2.tidyverse.org/reference/lims.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``22``, ``25``)``)`` ``+`\
`    `[`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)`(``colour ``=`` ``"black"``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"1984-05-16"``)``, y ``=`` ``24.5``,`\
`                label ``=`` ``"heat\n\n\n\n\nspike"``)``)`

![Comparison plot of geom_flame() event detection with default settings
shaded in black versus a 5-day minimum duration allowing a 2-day gap
shaded in red, illustrating how gap-joining connects a short temperature
spike into a single
event.](detection_and_visualisation_files/figure-html/flame_gaps-1.png)

Should we not wish to highlight any events with
[`geom_lolli()`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md),
plot them with a colour other than the default, and use a different
theme, it would look like this:

\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mhw``$``event``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``date_peak``, y ``=`` ``intensity_max``)``)`` ``+`\
`  `[`geom_lolli`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)`(``colour ``=`` ``"firebrick"``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Peak Date"``, `\
`       y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Max. intensity ["``, ``degree``, ``"C]"``)``)``, x ``=`` ``NULL``)`` ``+`\
`  `[`theme_linedraw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

![Lollipop plot of marine heatwave maximum intensity by peak date,
styled in firebrick red on a line-drawing ggplot2 theme, with no events
specially
highlighted.](detection_and_visualisation_files/figure-html/fig-example4-1.png)

Because these are simple **`ggplot2`** geoms possibilities are nearly
infinite.

## Calculating marine cold-spells (MCSs)

The calculation and visualisation of cold-spells is also provided for
within this package. The data to be fed into the functions is the same
as for MHWs. The main difference is that one is now calculating the 10th
percentile threshold, rather than the 90th percentile threshold. Here
are the top five cold-spells (cumulative intensity) detected in the
OISST data for Western Australia:

\
`# First calculate the cold-spells`\
`ts_10th`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_WA``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``, pctile ``=`` ``10``)`\
`mcs`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``ts_10th``, coldSpells ``=`` ``TRUE``)`\
\
`# Then look at the top few events`\
`mcs``$``event`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``event_no``, ``duration``, ``date_start``,`\
`                ``date_peak``, ``intensity_mean``, ``intensity_max``, ``intensity_cumulative``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``intensity_cumulative``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``5``)`

    ##   event_no duration date_start  date_peak intensity_mean intensity_max
    ## 1       15       76 1990-04-13 1990-05-11        -2.5027       -3.1883
    ## 2       49       58 2003-12-19 2004-01-23        -1.7341       -2.5865
    ## 3       83       41 2020-04-26 2020-05-25        -2.3374       -3.1433
    ## 4       64       52 2014-04-14 2014-05-05        -1.7824       -2.5358
    ## 5       77       46 2018-07-24 2018-08-02        -1.8096       -2.4311
    ##   intensity_cumulative
    ## 1            -190.2043
    ## 2            -100.5806
    ## 3             -95.8339
    ## 4             -92.6844
    ## 5             -83.2407

## Visualising marine cold-spells (MCSs)

### Default MCS visuals

The default plots showing cold-spells look like this:

\
[`event_line`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)`(``mcs``, spread ``=`` ``200``, metric ``=`` ``intensity_cumulative``,`\
`           start_date ``=`` ``"1982-01-01"``, end_date ``=`` ``"2014-12-31"``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.14``, ``0.13``)``,`\
`        legend.direction ``=`` ``"vertical"``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Line plot of Western Australia sea surface temperature from late 1989
to early 1991 showing the climatology, the 10th-percentile threshold,
and the most cumulatively intense marine cold-spell shaded in
blue.](detection_and_visualisation_files/figure-html/fig-example5-1.png)

\
[`lolli_plot`](https://robwschlegel.github.io/heatwaveR/reference/lolli_plot.md)`(``mcs``, metric ``=`` ``intensity_cumulative``, xaxis ``=`` ``event_no``)`

![Lollipop plot of all detected Western Australia marine cold-spell
events ordered by event number, with each event's cumulative intensity
shown as a
stem.](detection_and_visualisation_files/figure-html/fig-example5-2.png)

Note that one does not need to specify that MCSs are to be visualised,
the functions are able to understand this on their own.

### Custom MCS visuals

Cold spell figures may be created as `geoms` in **`ggplot2`**, too:

\
`# Select the region of the time series of interest`\
`mcs2`` ``<-`` ``mcs``$``climatology`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``2900``:``3190``)`\
\
`# Note that one must specify a colour other than the default 'salmon'`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mcs2``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``thresh``, y2 ``=`` ``temp``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``fill ``=`` ``"steelblue3"``)`

![Custom ggplot2 geom_flame plot of temperature below the
10th-percentile threshold during a Western Australia marine cold-spell,
shaded in steel
blue.](detection_and_visualisation_files/figure-html/fig-example6-1.png)

\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mcs``$``event``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``date_start``, y ``=`` ``intensity_max``)``)`` ``+`\
`  `[`geom_lolli`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)`(``colour ``=`` ``"steelblue3"``, colour_n ``=`` ``"navy"``, n ``=`` ``3``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Start Date"``,`\
`       y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Max. intensity ["``, ``degree``, ``"C]"``)``)``)`

![Custom ggplot2 geom_lolli plot of marine cold-spell maximum intensity
by start date, with the three most intense cold-spells highlighted in
navy.](detection_and_visualisation_files/figure-html/fig-example6-2.png)

### Minty MCS visuals

Again, because
[`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)
and
[`geom_lolli()`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)
are simple **`ggplot2`** geoms, one can go completely bananas with them:

\
`mcs_top`` ``<-`` ``mcs2`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``125``:``202``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``mcs2``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, y2 ``=`` ``temp``, fill ``=`` ``"all"``)``, show.legend ``=`` ``T``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(``data ``=`` ``mcs_top``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, y2 ``=`` ``temp``, fill ``=`` ``"top"``)``, show.legend ``=`` ``T``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, colour ``=`` ``"temp"``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, colour ``=`` ``"thresh"``)``, size ``=`` ``1.0``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``seas``, colour ``=`` ``"seas"``)``, size ``=`` ``1.2``)`` ``+`\
`  `[`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``"Line Colour"``,`\
`                      values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"temp"`` ``=`` ``"black"``, ``"thresh"`` ``=``  ``"forestgreen"``, ``"seas"`` ``=`` ``"grey80"``)``)`` ``+`\
`  `[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``"Event Colour"``, values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"all"`` ``=`` ``"steelblue3"``, ``"top"`` ``=`` ``"navy"``)``)`` ``+`\
`  `[`scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)`(``date_labels ``=`` ``"%b %Y"``)`` ``+`\
`  `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``override.aes ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``fill ``=`` ``NA``)``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Temperature ["``, ``degree``, ``"C]"``)``)``, x ``=`` ``NULL``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.12``, ``0.3``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Fully custom ggplot2 reproduction of a marine cold-spell figure,
showing temperature, the seasonal climatology, and the 10th-percentile
threshold as lines, with the full cold-spell event shaded in steel blue
and its most intense portion shaded in navy, each with a colour
legend.](detection_and_visualisation_files/figure-html/fig-example7-1.png)

\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mcs``$``event``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``date_start``, y ``=`` ``intensity_cumulative``)``)`` ``+`\
`  `[`geom_lolli`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)`(``colour ``=`` ``"steelblue3"``, colour_n ``=`` ``"navy"``, n ``=`` ``7``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`` x ``=`` ``"Start Date"``, y ``=`` `[`expression`](https://rdrr.io/r/base/expression.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``"Cumulative intensity [days x "``, ``degree``, ``"C]"``)``)``)`

![Lollipop plot of marine cold-spell cumulative intensity by start date,
with the seven most intense cold-spells highlighted in
navy.](detection_and_visualisation_files/figure-html/fig-example7-2.png)

## Interactive visuals

As of **`heatwaveR`** v0.3.6.9002,
[`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)
was also able to be used with **`plotly`** to allow for interactive MHW
visuals. Unfortunately around December of 2020 the **`plotly`** packaged
was orphaned and CRAN decided it didn’t want packages to include it as
an imported package. Therefore as of v0.4.4.9005 **`heatwaveR`** no
longer has built in support for using
[`geom_flame()`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)
with **`plotly`**. It is however still possible with a bit of work and a
simple working example is given below. It is not currently possible to
use
[`geom_lolli()`](https://robwschlegel.github.io/heatwaveR/reference/geom_lolli.md)
with **`plotly`**. Rather one is advised to just create the dots and
segments separately with
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
and
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
respectively as these are already recognised by **`plotly`**.

Note that the following code chunk is not run as it makes this vignette
a bit too large.

\
`# Must load plotly library first`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`plotly`](https://plotly-r.com)`)`\
\
`# Function needed for making geom_flame() work with plotly`\
`geom2trace.GeomFlame`` ``<-`` ``function`` ``(``data``,`\
`                                  ``params``,`\
`                                  ``p``)`` ``{`\
`  `\
`  ``x`` ``<-`` ``y`` ``<-`` ``y2`` ``<-`` ``NULL`\
`  `\
`  ``# Create data.frame for ease of use`\
`  ``data1`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``x ``=`` ``data``[[``"x"``]``]``,`\
`                      y ``=`` ``data``[[``"y"``]``]``,`\
`                      y2 ``=`` ``data``[[``"y2"``]``]``)`\
`  `\
`  ``# Grab parameters`\
`  ``n`` ``<-`` ``params``[[``"n"``]``]`\
`  ``n_gap`` ``<-`` ``params``[[``"n_gap"``]``]`\
`  `\
`  ``# Find events that meet minimum length requirement`\
`  ``data_event`` ``<-`` ``heatwaveR``::`[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``data1``, x ``=`` ``x``, y ``=`` ``y``,`\
`                                        seasClim ``=`` ``y``,`\
`                                        threshClim ``=`` ``y2``,`\
`                                        minDuration ``=`` ``n``,`\
`                                        maxGap ``=`` ``n_gap``,`\
`                                        protoEvents ``=`` ``T``)`\
`  `\
`  ``# Detect spikes`\
`  ``data_event``$``screen`` ``<-`` ``base``::`[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``data_event``$``threshCriterion`` ``==`` ``FALSE``, ``FALSE``,`\
`                                    `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``data_event``$``event`` ``==`` ``FALSE``, ``TRUE``, ``FALSE``)``)`\
`  `\
`  ``# Screen out spikes`\
`  ``data1`` ``<-`` ``data1``[``data_event``$``screen`` ``!=`` ``TRUE``,``]`\
`  `\
`  ``# Prepare to find the polygon corners`\
`  ``x1`` ``<-`` ``data1``$``y`\
`  ``x2`` ``<-`` ``data1``$``y2`\
`  `\
`  ``# # Find points where x1 is above x2.`\
`  ``above`` ``<-`` ``x1`` ``>`` ``x2`\
`  ``above``[``above`` ``==`` ``TRUE``]`` ``<-`` ``1`\
`  ``above``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(``above``)``]`` ``<-`` ``0`\
`  `\
`  ``# Points always intersect when above=TRUE, then FALSE or reverse`\
`  ``intersect.points`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`diff`](https://rdrr.io/r/base/diff.html)`(``above``)`` ``!=`` ``0``)`\
`  `\
`  ``# Find the slopes for each line segment.`\
`  ``x1.slopes`` ``<-`` ``x1``[``intersect.points`` ``+`` ``1``]`` ``-`` ``x1``[``intersect.points``]`\
`  ``x2.slopes`` ``<-`` ``x2``[``intersect.points`` ``+`` ``1``]`` ``-`` ``x2``[``intersect.points``]`\
`  `\
`  ``# # Find the intersection for each segment.`\
`  ``x.points`` ``<-`` ``intersect.points`` ``+`` ``(``(``x2``[``intersect.points``]`` ``-`` ``x1``[``intersect.points``]``)`` ``/`` ``(``x1.slopes`` ``-`` ``x2.slopes``)``)`\
`  ``y.points`` ``<-`` ``x1``[``intersect.points``]`` ``+`` ``(``x1.slopes`` ``*`` ``(``x.points`` ``-`` ``intersect.points``)``)`\
`  `\
`  ``# Coerce x.points to the same scale as x`\
`  ``x_gap`` ``<-`` ``data1``$``x``[``2``]`` ``-`` ``data1``$``x``[``1``]`\
`  ``x.points`` ``<-`` ``data1``$``x``[``intersect.points``]`` ``+`` ``(``x_gap``*``(``x.points`` ``-`` ``intersect.points``)``)`\
`  `\
`  ``# Create new data frame and merge to introduce new rows of data`\
`  ``data2`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``data1``$``y``, ``y.points``)``, x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``data1``$``x``, ``x.points``)``)`\
`  ``data2`` ``<-`` ``data2``[`[`order`](https://rdrr.io/r/base/order.html)`(``data2``$``x``)``,``]`\
`  ``data3`` ``<-`` ``base``::`[`merge`](https://rdrr.io/r/base/merge.html)`(``data1``, ``data2``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``,``"y"``)``, all.y ``=`` ``T``)`\
`  ``data3``$``y2``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(``data3``$``y2``)``]`` ``<-`` ``data3``$``y``[`[`is.na`](https://rdrr.io/r/base/NA.html)`(``data3``$``y2``)``]`\
`  `\
`  ``# Remove missing values for better plotting`\
`  ``data3``$``y``[``data3``$``y`` ``<`` ``data3``$``y2``]`` ``<-`` ``NA`\
`  ``missing_pos`` ``<-`` ``!``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``data3``[`[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``, ``"y2"``)``]``)`\
`  ``ids`` ``<-`` `[`cumsum`](https://rdrr.io/r/base/cumsum.html)`(``missing_pos``)`` ``+`` ``1`\
`  ``ids``[``missing_pos``]`` ``<-`` ``NA`\
`  `\
`  ``# Get the correct positions`\
`  ``positions`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``data3``$``x``, `[`rev`](https://rdrr.io/r/base/rev.html)`(``data3``$``x``)``)``,`\
`                          y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``data3``$``y``, `[`rev`](https://rdrr.io/r/base/rev.html)`(``data3``$``y2``)``)``,`\
`                          ids ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``ids``, `[`rev`](https://rdrr.io/r/base/rev.html)`(``ids``)``)``)`\
`  `\
`  ``# Convert to a format geom2trace is happy with`\
`  ``positions`` ``<-`` ``plotly``::``group2NA``(``positions``, groupNames ``=`` ``"ids"``)`\
`  ``positions`` ``<-`` ``positions``[``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``positions``$``ids``)``,``]`\
`  ``positions`` ``<-`` ``dplyr``::`[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``positions``, ``data``[``,``-`[`c`](https://rdrr.io/r/base/c.html)`(``2``,``3``)``]``, by ``=`` ``"x"``)`\
`  ``if``(`[`length`](https://rdrr.io/r/base/length.html)`(``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``positions``$``PANEL``)``)`` ``>`` ``1``)`` `\
`    ``positions``$``PANEL`` ``<-`` ``positions``$``PANEL``[``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``positions``$``PANEL``)``]``[``1``]`\
`  ``if``(`[`length`](https://rdrr.io/r/base/length.html)`(``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``positions``$``group``)``)`` ``>`` ``1``)`` `\
`    ``positions``$``group`` ``<-`` ``positions``$``group``[``stats``::`[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)`(``positions``$``group``)``]``[``1``]`\
`  `\
`  ``# Run the plotly polygon code`\
`  ``if``(`[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``positions``$``PANEL``)``)`` ``==`` ``1``)``{`\
`    `[`getFromNamespace`](https://rdrr.io/r/utils/getFromNamespace.html)`(``"geom2trace.GeomPolygon"``, `[`asNamespace`](https://rdrr.io/r/base/ns-internal.html)`(``"plotly"``)``)``(``positions``)`\
`  ``}`` ``else``{`\
`    `[`return`](https://rdrr.io/r/base/function.html)`(``)`\
`  ``}`\
`}`\
\
`# Time series`\
`ts_res`` ``<-`` ``heatwaveR``::`[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``data ``=`` ``heatwaveR``::`[`sst_WA`](https://robwschlegel.github.io/heatwaveR/reference/sst_WA.md)`,`\
`                            climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)`\
`ts_res_sub`` ``<-`` ``ts_res``[``10500``:``10800``,``]`\
\
`# Flame Figure`\
`p`` ``<-`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``ts_res_sub``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``temp``)``)`` ``+`\
`  ``heatwaveR``::`[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y2 ``=`` ``thresh``)``, n ``=`` ``5``, n_gap ``=`` ``2``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``seas``)``, colour ``=`` ``"green"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``)``, colour ``=`` ``"red"``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``""``, y ``=`` ``"Temperature (°C)"``)`\
\
`# Create interactive visuals`\
`ggplotly``(``p``)`
