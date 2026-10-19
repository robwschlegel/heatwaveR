# Calculating and Visualising Event Categories

## Categories

In Hobday et al. (2018) a naming convention for MHWs was proposed that
divides them into four categories based on their maximum observed
intensity. The naming convention and a brief description are as follows:

| Category | Description |
|----|----|
| I Moderate | Events that have been detected, but with a maximum intensity that does not double the distance between the seasonal climatology and the threshold value. These are common and not terribly worrisome. |
| II Strong | Events with a maximum intensity that doubles the distance from the seasonal climatology to the threshold, but does not triple it. These are not uncommon, but have yet to be shown to cause any long term biological or ecological damage. |
| III Severe | Thankfully these are relatively uncommon as they have been linked to damaging events. The 2003 Mediterranean MHW was this category. |
| IV Extreme | Events with a maximum intensity that is four times or greater than the aforementioned distance. These events are currently rare, but are projected to increase with frequency. This is troubling as events in this category are now well documented as causing widespread and lasting ecological damage. The 2011 Western Australia MHW was this category. It is also the logo of this package. |

## Calculating MHW categories

The categories of MHWs under the Hobday et al. (2018) naming scheme may
be calculated with the **`heatwaveR`** package using the
[`category()`](https://robwschlegel.github.io/heatwaveR/reference/category.md)
function on the output of the
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
function. By default this function will order events from most to least
intense. Note that one may control the output for the names of the
events by providing ones own character string for the `name` argument.
Because we have calculated MHWs on the Western Australia data, we
provide the name “WA” below:

\
`# Load libraries`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyr`](https://tidyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`heatwaveR`](https://robwschlegel.github.io/heatwaveR/)`)`\
\
`# Calculate events`\
`ts`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_WA``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)`\
`MHW`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``ts``)`` `\
`MHW_cat`` ``<-`` `[`category`](https://robwschlegel.github.io/heatwaveR/reference/category.md)`(``MHW``, S ``=`` ``TRUE``, name ``=`` ``"WA"``)`\
\
`# Look at the top few events`\
[`tail`](https://rdrr.io/r/utils/head.html)`(``MHW_cat``)`

    ##    event_no event_name  peak_date   category  i_max duration p_moderate
    ## 85       60   WA 2012b 2012-12-31  II Strong 3.4230       14         64
    ## 86       29    WA 1999 1999-05-22  II Strong 3.6390       95         63
    ## 87       47    WA 2009 2009-03-25  II Strong 2.3773        7         57
    ## 88       72    WA 2015 2015-10-02  II Strong 2.4604        7         57
    ## 89       41   WA 2008a 2008-04-14 III Severe 3.8299       35         57
    ## 90       52   WA 2011a 2011-02-28 IV Extreme 6.5798      105         52
    ##    p_strong p_severe p_extreme      season
    ## 85       36        0         0      Summer
    ## 86       37        0         0 Fall/Winter
    ## 87       43        0         0        Fall
    ## 88       43        0         0      Spring
    ## 89       23       17         0        Fall
    ## 90       27       11        10 Summer/Fall

Note that this functions expects the data to have been collected in the
southern hemisphere, hence the argument `S = TRUE`. If they were not,
one must set `S = FALSE` as seen in the example below. This ensures that
the correct seasons are attributed to the event.

\
`res_Med`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(`[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_Med``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)``)`\
`res_Med_cat`` ``<-`` `[`category`](https://robwschlegel.github.io/heatwaveR/reference/category.md)`(``res_Med``, S ``=`` ``FALSE``, name ``=`` ``"Med"``)`\
[`tail`](https://rdrr.io/r/utils/head.html)`(``res_Med_cat``)`

    ##     event_no event_name  peak_date   category  i_max duration p_moderate
    ## 118       98  Med 2018b 2018-08-04  II Strong 4.7451       44         52
    ## 119      123  Med 2022c 2022-11-03  II Strong 3.5030       74         46
    ## 120       67  Med 2012b 2012-08-20  II Strong 4.3190       18         44
    ## 121       46  Med 2007c 2007-04-25 III Severe 4.0467       19         42
    ## 122       75   Med 2014 2014-10-18  II Strong 3.3443      144         39
    ## 123       96  Med 2018a 2018-04-28  II Strong 3.3185       11         27
    ##     p_strong p_severe p_extreme      season
    ## 118       48        0         0 Summer/Fall
    ## 119       53        0         0 Fall/Winter
    ## 120       56        0         0      Summer
    ## 121       53        5         0      Spring
    ## 122       60        0         0 Fall/Winter
    ## 123       73        0         0      Spring

## Multiple time series

If however we may want to determine the categories of event within a set
of time series that cross over the equator, we may set the argument
`lat_col = TRUE` to allow
[`category()`](https://robwschlegel.github.io/heatwaveR/reference/category.md)
to automagically detect the latitude of the events by searching for
columns named either ‘lat’ or ’latitude. See the vignette for [detecting
events in gridded
data](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html)
for more examples of running **`heatwaveR`** code on gridded data.

\
`# Add lon/lat to the three default time series`\
`ts_WA`` ``<-`` ``sst_WA`` ``|>`` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``site ``=`` ``"WA"``, lon ``=`` ``112.625``, lat ``=`` ``-``29.375``)`\
`ts_NW_Atl`` ``<-`` ``sst_NW_Atl`` ``|>`` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``site ``=`` ``"NW_Atl"``, lon ``=`` ``-``66.875``, lat ``=`` ``43.125``)`\
`ts_Med`` ``<-`` ``sst_Med`` ``|>`` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``site ``=`` ``"Med"``, lon ``=`` ``9.125``, lat ``=`` ``43.625``)`\
`ts_ALL`` ``<-`` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(``ts_WA``, ``ts_NW_Atl``, ``ts_Med``)`\
\
`# Calculate MHW categories by site`\
`MHW_cat_ALL`` ``<-`` ``ts_ALL`` ``|>`` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``site``)`` ``|>`` `\
`  `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~`` ``{`\
`    ``.x`` ``|>`` `\
`      `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)`` ``|>`` `\
`      `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``)`` ``|>`` `\
`      `[`category`](https://robwschlegel.github.io/heatwaveR/reference/category.md)`(``season ``=`` ``"peak"``, lat_col ``=`` ``TRUE``)`\
`    ``}``)`` ``|>`` `\
`  ``# Correct event names by site`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``event_name ``=`` `[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``event_name``)`` ``~`` ``stringr``::`[`str_replace`](https://stringr.tidyverse.org/reference/str_replace.html)`(``event_name``, ``"Event"``, ``site``)``)``)`\
\
`# View results`\
`MHW_cat_ALL`` ``|>`` `\
`  `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``-``duration``)`` ``|>`` `\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``event_name``)``)`` ``|>`` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``site``)`` ``|>`` `\
`  `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~`` `[`head`](https://rdrr.io/r/utils/head.html)`(``.x``, ``2L``)``)`` ``|>`` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``site``:``category``, ``duration``, ``season``)`

    ## # A tibble: 6 × 7
    ## # Groups:   site [3]
    ##   site   event_no event_name   peak_date  category   duration season
    ##   <chr>     <int> <chr>        <date>     <chr>         <dbl> <chr> 
    ## 1 Med          75 Med 2014     2014-10-18 II Strong       144 Fall  
    ## 2 Med         122 Med 2022b    2022-07-20 II Strong        79 Summer
    ## 3 NW_Atl       72 NW_Atl 2012c 2012-07-15 II Strong       239 Summer
    ## 4 NW_Atl      100 NW_Atl 2018a 2018-01-12 III Severe      171 Winter
    ## 5 WA           52 WA 2011a     2011-02-28 IV Extreme      105 Summer
    ## 6 WA           59 WA 2012a     2012-01-27 II Strong       101 Summer

## Visualising MHW categories

### Default MHW category visuals

A quick and easy visualisation of the categories of a MHW may be
accomplished with
[`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)
by setting the `category` argument to `TRUE`.

\
[`event_line`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)`(``MHW``, spread ``=`` ``100``, start_date ``=`` ``"2010-11-01"``, end_date ``=`` ``"2011-06-30"``, category ``=`` ``TRUE``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.13``, ``0.35``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Category-coloured event_line plot of the 2011 Western Australia marine
heatwave from October 2010 to July 2011, with temperature shaded by
Hobday et al. (2018) category severity from Moderate to
Extreme.](event_categories_files/figure-html/fig-example-1-1.png)

### Custom MHW category visuals

Were one to want to visualise the categories of a MHW ‘by hand’, the
following code will provide a good starting point.

\
`# Create category breaks and select slice of data.frame`\
`clim_cat`` ``<-`` ``MHW``$``clim`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``diff ``=`` ``thresh`` ``-`` ``seas``,`\
`                thresh_2x ``=`` ``thresh`` ``+`` ``diff``,`\
`                thresh_3x ``=`` ``thresh_2x`` ``+`` ``diff``,`\
`                thresh_4x ``=`` ``thresh_3x`` ``+`` ``diff``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``10580``:``10690``)`\
\
`# Set line colours`\
`lineColCat`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Temperature"`` ``=`` ``"black"``,`\
`  ``"Climatology"`` ``=`` ``"gray20"``,`\
`  ``"Threshold"`` ``=`` ``"darkgreen"``,`\
`  ``"2x Threshold"`` ``=`` ``"darkgreen"``,`\
`  ``"3x Threshold"`` ``=`` ``"darkgreen"``,`\
`  ``"4x Threshold"`` ``=`` ``"darkgreen"`\
`  ``)`\
\
`# Set category fill colours`\
`fillColCat`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Moderate"`` ``=`` ``"#ffc866"``,`\
`  ``"Strong"`` ``=`` ``"#ff6900"``,`\
`  ``"Severe"`` ``=`` ``"#9e0000"``,`\
`  ``"Extreme"`` ``=`` ``"#2d0000"`\
`  ``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``clim_cat``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``temp``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y2 ``=`` ``thresh``, fill ``=`` ``"Moderate"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y2 ``=`` ``thresh_2x``, fill ``=`` ``"Strong"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y2 ``=`` ``thresh_3x``, fill ``=`` ``"Severe"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y2 ``=`` ``thresh_4x``, fill ``=`` ``"Extreme"``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_2x``, col ``=`` ``"2x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dashed"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_3x``, col ``=`` ``"3x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dotdash"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_4x``, col ``=`` ``"4x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dotted"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``seas``, col ``=`` ``"Climatology"``)``, size ``=`` ``0.7``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, col ``=`` ``"Threshold"``)``, size ``=`` ``0.7``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, col ``=`` ``"Temperature"``)``, size ``=`` ``0.6``)`` ``+`\
`  `[`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``NULL``, values ``=`` ``lineColCat``,`\
`                      breaks ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Temperature"``, ``"Climatology"``, ``"Threshold"``,`\
`                                 ``"2x Threshold"``, ``"3x Threshold"``, ``"4x Threshold"``)``)`` ``+`\
`  `[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``NULL``, values ``=`` ``fillColCat``, guide ``=`` ``FALSE``)`` ``+`\
`  `[`scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)`(``date_labels ``=`` ``"%b %Y"``)`` ``+`\
`  `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``override.aes ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``linetype ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"solid"``, ``"solid"``, ``"solid"``,`\
`                                                                ``"dashed"``, ``"dotdash"``, ``"dotted"``)``,`\
`                                                   size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.6``, ``0.7``, ``0.7``, ``0.7``, ``0.7``, ``0.7``)``)``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``y ``=`` ``"Temperature [°C]"``, x ``=`` ``NULL``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.12``, ``0.68``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Custom ggplot2 plot of the 2011 Western Australia marine heatwave
showing temperature against four category thresholds, shaded from amber
(Moderate) through orange and dark red to near-black (Extreme), with the
climatology and threshold lines
overlaid.](event_categories_files/figure-html/fig-example-2-1.png)

## Calculating MCS categories

MCSs are calculated the same as for MHWs. The
[`category()`](https://robwschlegel.github.io/heatwaveR/reference/category.md)
function will automagically detect if it has been fed MHWs or MCSs so no
additional arguments are required. For the sake of clarity the following
code chunks demonstrates how to calculate MCS categories.

\
`# Calculate events`\
`ts_MCS`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_WA``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``, pctile ``=`` ``10``)`\
`MCS`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``ts_MCS``, coldSpells ``=`` ``T``)`\
`MCS_cat`` ``<-`` `[`category`](https://robwschlegel.github.io/heatwaveR/reference/category.md)`(``MCS``, S ``=`` ``TRUE``, name ``=`` ``"WA"``)`\
\
`# Look at the top few events`\
[`tail`](https://rdrr.io/r/utils/head.html)`(``MCS_cat``)`

    ##    event_no event_name  peak_date  category   i_max duration p_moderate
    ## 81       77   WA 2018a 2018-08-02 II Strong -2.4311       46         67
    ## 82       40    WA 2000 2000-08-13 II Strong -2.2743       11         64
    ## 83       15    WA 1990 1990-05-11 II Strong -3.1883       76         62
    ## 84       53    WA 2005 2005-10-16 II Strong -1.8637       13         62
    ## 85       83    WA 2020 2020-05-25 II Strong -3.1433       41         61
    ## 86       11    WA 1987 1987-12-10 II Strong -2.4968        9         44
    ##    p_strong p_severe p_extreme        season
    ## 81       33        0         0 Winter/Spring
    ## 82       36        0         0        Winter
    ## 83       38        0         0   Fall/Winter
    ## 84       38        0         0        Spring
    ## 85       39        0         0   Fall/Winter
    ## 86       56        0         0        Summer

## Visualising MCS categories

### Default MCS category visuals

The
[`event_line()`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)
function also works for visualising MCS categories. The function will
automagically detect that it is being fed MCSs so we do not need to
provide it with any new arguments. Note that the colour palette for MCS
does have four colours, same as for MHWs, but none of the demo time
series that come packaged with **`heatwaveR`** have MCSs that intense so
we are not able to demonstrate the full colour palette here.

\
[`event_line`](https://robwschlegel.github.io/heatwaveR/reference/event_line.md)`(``MCS``, spread ``=`` ``100``, start_date ``=`` ``"1989-11-01"``, end_date ``=`` ``"1990-06-30"``, category ``=`` ``TRUE``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.13``, ``0.22``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Category-coloured event_line plot of a Western Australia marine
cold-spell from January to October 1990, with temperature shaded by
category severity using the cold-spell colour
scale.](event_categories_files/figure-html/fig-example-3-1.png)

### Custom MCS category visuals

The following code chunk demonstrates how to manually create a figure
showing the MCS categories.

\
`# Create category breaks and select slice of data.frame`\
`MCS_clim_cat`` ``<-`` ``MCS``$``clim`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``dplyr``::`[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``diff ``=`` ``thresh`` ``-`` ``seas``,`\
`                thresh_2x ``=`` ``thresh`` ``+`` ``diff``,`\
`                thresh_3x ``=`` ``thresh_2x`` ``+`` ``diff``,`\
`                thresh_4x ``=`` ``thresh_3x`` ``+`` ``diff``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``2910``:``3150``)`\
\
`# Set line colours`\
`lineColCat`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Temperature"`` ``=`` ``"black"``,`\
`  ``"Climatology"`` ``=`` ``"grey40"``,`\
`  ``"Threshold"`` ``=`` ``"darkorchid"``,`\
`  ``"2x Threshold"`` ``=`` ``"darkorchid"``,`\
`  ``"3x Threshold"`` ``=`` ``"darkorchid"``,`\
`  ``"4x Threshold"`` ``=`` ``"darkorchid"`\
`  ``)`\
\
`# Set category fill colours`\
`fillColCat`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Moderate"`` ``=`` ``"#C7ECF2"``,`\
`  ``"Strong"`` ``=`` ``"#85B7CC"``,`\
`  ``"Severe"`` ``=`` ``"#4A6A94"``,`\
`  ``"Extreme"`` ``=`` ``"#111433"`\
`  ``)`\
\
`# Create plot`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``MCS_clim_cat``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``t``, y ``=`` ``temp``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, y2 ``=`` ``temp``, fill ``=`` ``"Moderate"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_2x``, y2 ``=`` ``temp``, fill ``=`` ``"Strong"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_3x``, y2 ``=`` ``temp``, fill ``=`` ``"Severe"``)``)`` ``+`\
`  `[`geom_flame`](https://robwschlegel.github.io/heatwaveR/reference/geom_flame.md)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_4x``, y2 ``=`` ``temp``, fill ``=`` ``"Extreme"``)``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_2x``, col ``=`` ``"2x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dashed"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_3x``, col ``=`` ``"3x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dotdash"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh_4x``, col ``=`` ``"4x Threshold"``)``, size ``=`` ``0.7``, linetype ``=`` ``"dotted"``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``seas``, col ``=`` ``"Climatology"``)``, size ``=`` ``0.7``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``thresh``, col ``=`` ``"Threshold"``)``, size ``=`` ``0.7``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``y ``=`` ``temp``, col ``=`` ``"Temperature"``)``, size ``=`` ``0.6``)`` ``+`\
`  `[`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``NULL``, values ``=`` ``lineColCat``,`\
`                      breaks ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Temperature"``, ``"Climatology"``, ``"Threshold"``,`\
`                                 ``"2x Threshold"``, ``"3x Threshold"``, ``"4x Threshold"``)``)`` ``+`\
`  `[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``name ``=`` ``NULL``, values ``=`` ``fillColCat``, guide ``=`` ``FALSE``)`` ``+`\
`  `[`scale_x_date`](https://ggplot2.tidyverse.org/reference/scale_date.html)`(``date_labels ``=`` ``"%b %Y"``)`` ``+`\
`  `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``override.aes ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``linetype ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"solid"``, ``"solid"``, ``"solid"``,`\
`                                                                ``"dashed"``, ``"dotdash"``, ``"dotted"``)``,`\
`                                                   size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.6``, ``0.7``, ``0.7``, ``0.7``, ``0.7``, ``0.7``)``)``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``y ``=`` ``"Temperature [°C]"``, x ``=`` ``NULL``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"inside"``, legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.13``, ``0.22``)``,`\
`        legend.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``fill ``=`` ``"white"``, colour ``=`` ``"grey50"``)``)`

![Custom ggplot2 plot of a Western Australia marine cold-spell showing
temperature against four category thresholds, shaded from pale blue
(Moderate) through steel blue and slate blue to near-black (Extreme),
with the climatology and threshold lines
overlaid.](event_categories_files/figure-html/fig-example-4-1.png)

## Category colour palettes

For the sake of convenience the MHW and MCS colour palettes are provided
below with a figure showing the direct comparison.

\
`# The MCS colour palette`\
`MCS_colours`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Moderate"`` ``=`` ``"#C7ECF2"``,`\
`  ``"Strong"`` ``=`` ``"#85B7CC"``,`\
`  ``"Severe"`` ``=`` ``"#4A6A94"``,`\
`  ``"Extreme"`` ``=`` ``"#111433"`\
`)`\
\
`# The MHW colour palette`\
`MHW_colours`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  ``"Moderate"`` ``=`` ``"#ffc866"``,`\
`  ``"Strong"`` ``=`` ``"#ff6900"``,`\
`  ``"Severe"`` ``=`` ``"#9e0000"``,`\
`  ``"Extreme"`` ``=`` ``"#2d0000"`\
`)`\
\
`# Create the colour palette for plotting by itself`\
`colour_palette`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``category ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"I Moderate"``, ``"II Strong"``, ``"III Severe"``, ``"IV Extreme"``)``,`\
`                                               levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"I Moderate"``, ``"II Strong"``, ``"III Severe"``, ``"IV Extreme"``)``)``,`\
`                             MHW ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``MHW_colours``[``1``]``, ``MHW_colours``[``2``]``, ``MHW_colours``[``3``]``, ``MHW_colours``[``4``]``)``,`\
`                             MCS ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``MCS_colours``[``1``]``, ``MCS_colours``[``2``]``, ``MCS_colours``[``3``]``, ``MCS_colours``[``4``]``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``cols ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``MHW``, ``MCS``)``, names_to ``=`` ``"event"``, values_to ``=`` ``"colour"``)`\
\
`# Show the palettes side-by-side`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``data ``=`` ``colour_palette``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``category``, y ``=`` ``event``)``)`` ``+`\
`  `[`geom_tile`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(``fill ``=`` ``colour_palette``$``colour``)`` ``+`\
`  `[`coord_cartesian`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)`(``expand ``=`` ``F``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``NULL``, y ``=`` ``NULL``)`

![Tile grid comparing the four-step Hobday et al. (2018) category colour
palettes for marine heatwaves (amber to near-black) and marine
cold-spells (pale blue to near-black), arranged from Moderate to
Extreme.](event_categories_files/figure-html/comp-fig-1.png)
