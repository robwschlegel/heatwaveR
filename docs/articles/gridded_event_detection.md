# Detecting Events in Gridded Data

## Overview

In the previous vignette we saw how to [download and prepare OISST
data](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html).
In this vignette we will use the subsetted data we downloaded (not
global) for our example on how to detect MHWs in gridded data. It is now
also possible to perform this task using
[**`heatwave3`**](https://robwschlegel.github.io/heatwave3/index.html) ,
which has been created to apply **`heatwaveR`** code directly to gridded
data files. It can return the results as CSV or NetCDF.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` ``# For basic data manipulation`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`` ``# For visualising data`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`heatwaveR`](https://robwschlegel.github.io/heatwaveR/)`)`` ``# For detecting MHWs`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidync`](https://docs.ropensci.org/tidync/)`)`` ``# For easily dealing with NetCDF data`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`doParallel`](https://github.com/RevolutionAnalytics/doparallel)`)`` ``# For parallel processing`

## Loading data

Because we saved our data as an `.Rds` file, loading it into R is easy.

\
`OISST`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(``"~/Desktop/OISST_vignette.Rds"``)`

## Event detection

### Two good choices: **`dplyr`** vs. **`plyr`**

When we want to make the same calculation across multiple groups of data
within one dataframe we have two good options available to us. The first
is to make use of the
[`map()`](https://purrr.tidyverse.org/reference/map.html) suite of
functions found in the **`purrr`** package, and now implemented in
**`dplyr`**. This is a very fast **`tidyverse`** friendly approach to
splitting up tasks. The other good option is to go back in time a bit
and use the `ddply()` function from the **`plyr`** package. This is
arguably a better approach as it allows us to very easily use multiple
cores to detect the MHWs. The problem with this approach is that one
must *never* load the **`plyr`** library directly as it has some
fundamental inconsistencies with the **`tidyverse`**. We will see below
how to perform these two different techniques without causing ourselves
any headaches.

It is a little clumsy to use multiple functions at once with the two
methods so we will combine the calculations we want to make into one
wrapper function.

\
`event_only`` ``<-`` ``function``(``df``)``{`\
`  ``# First calculate the climatologies`\
`  ``clim`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``data ``=`` ``df``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-01-01"``)``)`\
`  ``# Then the events`\
`  ``event`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``data ``=`` ``clim``)`\
`  ``# Return only the event metric dataframe of results`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(``event``$``event``)`\
`}`

#### The **`dplyr`** method

This method requires no special consideration and is performed just as
any other friendly **`tidyverse`** code chunk would be.

\
[`system.time`](https://rdrr.io/r/base/system.time.html)`(`\
`# First we start by choosing the 'OISST' dataframe`\
`MHW_dplyr`` ``<-`` ``OISST`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Then we group the data by the 'lon' and 'lat' columns`\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Then we run our MHW detecting function on each group`\
`  `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~``event_only``(``.x``)``)`\
`)`` ``# ~123 seconds`

Running the above calculations with only one of the 2.8 GHz cores on a
modern laptop took ~123 seconds. It must be noted however that a recent
update to the **`dplyr`** package now allows it to interrogate one’s
computer to determine how many cores it has at it’s disposal. It then
uses one core at full capacity and the other cores usually at half
capacity.

#### The **`plyr`** technique

This method requires that we first tell our machine how many of its
processor cores to give us for our calculation.

\
`# NB: One should never use ALL available cores, save at least 1 for other essential tasks`\
`# The computer I'm writing this vignette on has 8 cores, so I use 7 here`\
[`registerDoParallel`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html)`(``cores ``=`` ``7``)`\
\
`# Detect events`\
[`system.time`](https://rdrr.io/r/base/system.time.html)`(`\
`MHW_plyr`` ``<-`` ``plyr``::`[`ddply`](https://rdrr.io/pkg/plyr/man/ddply.html)`(``.data ``=`` ``OISST``, .variables ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"lon"``, ``"lat"``)``, .fun ``=`` ``event_only``, .parallel ``=`` ``TRUE``)`\
`)`` ``# 33 seconds`

The **`plyr`** technique took 33 seconds using seven cores. This
technique is not seven times faster because when using multiple cores
there is a certain amount of loss in efficiency due to the computer
needing to remember which results are meant to go where so that it can
stitch everything back together again for you. This takes very little
memory, but over large jobs it can start to become problematic.
Occasionally ‘slippage’ can occur as well where an entire task can be
forgotten. This is very rare but does happen. This is partly what makes
**`dplyr`** a viable option as it does not have this problem. The other
reason is that **`dplyr`** performs more efficient calculations than
**`plyr`**. But what if we could have the best of both worlds?

### A harmonious third option

As one may see above, running these calculations on a very large (or
even global) gridded dataset can quickly become very heavy. While
running these calculations myself on the global OISST dataset I have
found that the fastest option is to combine the two options above. In my
workflow I have saved each longitude segment of the global OISST dataset
as separate files and use the **`dplyr`** method on each individual
file, while using the **`plyr`** method to be running the multiple
calculations on as many files as my core limit will allow. One may not
do this the other way around and use **`dplyr`** to run multiple
**`plyr`** calculations at once. This will confuse your computer and
likely cause a stack overflow. Which sounds more fun than it actually
is… as I have had to learn.

In order to happily combine these two options into one we will need to
convert the **`dplyr`** code we wrote above into it’s own wrapper
function, which we will then call on a stack of files using the
**`plyr`** technique. Before we do that we must first create the
aforementioned stack of files.

\
`for``(``i`` ``in`` ``1``:`[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``OISST``$``lon``)``)``)``{`\
`  ``OISST_sub`` ``<-`` ``OISST`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``lon`` ``==`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``lon``)``[``i``]``)`\
`  `[`saveRDS`](https://rdrr.io/r/base/readRDS.html)`(``object ``=`` ``OISST_sub``, file ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"~/Desktop/OISST_lon_"``,``i``,``".Rds"``)``)`\
`}`

This may initially seem like an unnecessary extra step, but when one is
working with time series data it is necessary to have all of the dates
at a given pixel loaded at once. Unless one is working from a
server/virtual machine/supercomputer this means that one will often not
be able to comfortably hold an entire grid for a study area in memory at
once. Having the data accessible as thin strips like this makes life
easier. And as we see in the code chunk below it also (arguably) allows
us to perform the most efficient calculations on our data.

\
`# The 'dplyr' wrapper function to pass to 'plyr'`\
`dplyr_wraper`` ``<-`` ``function``(``file_name``)``{`\
`  ``MHW_dplyr`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(``file_name``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~``event_only``(``.x``)``)`\
`}`\
`# Create a vector of the files we want to use`\
`OISST_files`` ``<-`` `[`dir`](https://rdrr.io/r/base/list.files.html)`(``"~/Desktop"``, pattern ``=`` ``"OISST_lon_*"``, full.names ``=`` ``T``)`\
\
`# Use 'plyr' technique to run 'dplyr' technique with multiple cores`\
[`system.time`](https://rdrr.io/r/base/system.time.html)`(`\
`MHW_result`` ``<-`` ``plyr``::`[`ldply`](https://rdrr.io/pkg/plyr/man/ldply.html)`(``OISST_files``, .fun ``=`` ``dplyr_wraper``, .parallel ``=`` ``T``)`\
`)`` ``# 31 seconds`\
\
`# Save for later use as desired`\
[`saveRDS`](https://rdrr.io/r/base/readRDS.html)`(``MHW_result``, ``"~/Desktop/MHW_result.Rds"``)`

Even though this technique is not much faster computationally, it is
much lighter on our memory (RAM) as it only loads one longitude slice of
our data at a time. To maximise efficiency even further I would
recommend writing out this full workflow in a stand-alone script and
then running it using [`source()`](https://rdrr.io/r/base/source.html)
directly from an R terminal. The gain in speed here appears nominal, but
as one scales this up the speed boost becomes apparent.

As mentioned above, recent changes to how **`dplyr`** interacts with
one’s computer has perhaps slowed down the **`plyr`** + **`dplyr`**
workflow shown here. It may be now that simply using **`plyr`** by
itself is the better option. It depends on the number of cores and the
amount of RAM that one has available.

## Case study

Because of human-induced climate change, we anticipate that extreme
events will occur more frequently and that they will become greater in
intensity. Here we investigate this hypothesis by using gridded SST
data, which is the only way that we can assess if this trend is
unfolding across large ocean regions. Using the gridded 0.25 degree
Reynolds OISST, we will detect marine heatwaves (MHWs) around South
Africa by applying the
[`detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
function pixel-by-pixel to the data we downloaded in [the previous
vignette](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html).
After detecting the events, we will fit a generalised linear model (GLM)
to each pixel to calculate rates of change in some MHW metrics, and then
plot the estimated trends.

### Trend detection

With our MHW detected we will now look at how to fit some GLMs to the
results in order to determine long-term trends in MHW occurrence.

Up first we see how to calculate the number of events that occurred per
pixel.

\
`# summarise the number of unique longitude, latitude and year combination:`\
`OISST_n`` ``<-`` ``MHW_result`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``year ``=`` ``lubridate``::`[`year`](https://lubridate.tidyverse.org/reference/year.html)`(``date_start``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``lon``, ``lat``, ``year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(``n ``=`` `[`n`](https://dplyr.tidyverse.org/reference/context.html)`(``)``, .groups ``=`` ``"drop"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``tidyr``::`[`complete`](https://tidyr.tidyverse.org/reference/complete.html)`(``year ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1982``:``2019``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``# Note that these dates may differ`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``n ``=`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``n``)``, ``0``, ``n``)``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``OISST_n``)`

Then we specify the particulars of the GLM we are going to use.

\
`lin_fun`` ``<-`` ``function``(``ev``)`` ``{`\
`  ``mod1`` ``<-`` `[`glm`](https://rdrr.io/r/stats/glm.html)`(``n`` ``~`` ``year``, family ``=`` `[`poisson`](https://rdrr.io/r/stats/family.html)`(``link ``=`` ``"log"``)``, data ``=`` ``ev``)`\
`  ``# extract slope coefficient and its p-value`\
`  ``tr`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``slope ``=`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``mod1``)``$``coefficients``[``2``,``1``]``,`\
`                   p ``=`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``mod1``)``$``coefficients``[``2``,``4``]``)`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(``tr``)`\
`}`

Lastly we make the calculations.

\
`OISST_nTrend`` ``<-`` ``plyr``::`[`ddply`](https://rdrr.io/pkg/plyr/man/ddply.html)`(``OISST_n``, `[`c`](https://rdrr.io/r/base/c.html)`(``"lon"``, ``"lat"``)``, ``lin_fun``, .parallel ``=`` ``T``)`\
`OISST_nTrend``$``pval`` ``<-`` `[`cut`](https://rdrr.io/r/base/cut.html)`(``OISST_nTrend``$``p``, breaks ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0.001``, ``0.01``, ``0.05``, ``1``)``)`\
[`head`](https://rdrr.io/r/utils/head.html)`(``OISST_nTrend``)`

### Visualising the results

Let’s finish this vignette by visualising the long-term trends in the
annual occurrence of MHWs per pixel in the chosen study area. First we
will grab the base global map from the **`maps`** package.

\
`# The base map`\
`map_base`` ``<-`` ``ggplot2``::`[`fortify`](https://ggplot2.tidyverse.org/reference/fortify.html)`(``maps``::``map``(``fill ``=`` ``TRUE``, plot ``=`` ``FALSE``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``lon ``=`` ``long``)`

Then we will create two maps that we will stick together using
**`ggpubr`**. The first map will show the slope of the count of events
detected per year over time as shades of red, and the second map will
show the significance (*p*-value) of these trends in shades of grey.

\
`map_slope`` ``<-`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``OISST_nTrend``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``lon``, y ``=`` ``lat``)``)`` ``+`\
`  `[`geom_rect`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(``size ``=`` ``0.2``, fill ``=`` ``NA``,`\
`       `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``xmin ``=`` ``lon`` ``-`` ``0.1``, xmax ``=`` ``lon`` ``+`` ``0.1``, ymin ``=`` ``lat`` ``-`` ``0.1``, ymax ``=`` ``lat`` ``+`` ``0.1``,`\
`           colour ``=`` ``pval``)``)`` ``+`\
`  `[`geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``fill ``=`` ``slope``)``, interpolate ``=`` ``FALSE``, alpha ``=`` ``0.9``)`` ``+`\
`  `[`scale_fill_gradient2`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)`(``name ``=`` ``"count/year (slope)"``, high ``=`` ``"red"``, mid ``=`` ``"white"``,`\
`                       low ``=`` ``"darkblue"``, midpoint ``=`` ``0``,`\
`                       guide ``=`` `[`guide_colourbar`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)`(``direction ``=`` ``"horizontal"``,`\
`                                               title.position ``=`` ``"top"``)``)`` ``+`\
`  `[`scale_colour_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``breaks ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"(0,0.001]"``, ``"(0.001,0.01]"``, ``"(0.01,0.05]"``, ``"(0.05,1]"``)``,`\
`                      values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"firebrick1"``, ``"firebrick2"``, ``"firebrick3"``, ``"white"``)``,`\
`                      name ``=`` ``"p-value"``, guide ``=`` ``FALSE``)`` ``+`\
`  `[`geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.html)`(``data ``=`` ``map_base``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``group ``=`` ``group``)``, `\
`               colour ``=`` ``NA``, fill ``=`` ``"grey80"``)`` ``+`\
`  `[`coord_fixed`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)`(``ratio ``=`` ``1``, xlim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``13.0``, ``23.0``)``, ylim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``33``, ``-``42``)``, expand ``=`` ``TRUE``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``""``, y ``=`` ``""``)`` ``+`\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"bottom"``)`\
\
`map_p`` ``<-`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``OISST_nTrend``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``lon``, y ``=`` ``lat``)``)`` ``+`\
`  `[`geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``fill ``=`` ``pval``)``, interpolate ``=`` ``FALSE``)`` ``+`\
`  `[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)`(``breaks ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"(0,0.001]"``, ``"(0.001,0.01]"``, ``"(0.01,0.05]"``,`\
`                               ``"(0.05,0.1]"``, ``"(0.1,0.5]"``, ``"(0.5,1]"``)``,`\
`                    values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"black"``, ``"grey20"``, ``"grey40"``,`\
`                               ``"grey80"``, ``"grey90"``, ``"white"``)``,`\
`                    name ``=`` ``"p-value"``,`\
`                    guide ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``direction ``=`` ``"horizontal"``,`\
`                                               title.position ``=`` ``"top"``)``)`` ``+`\
`  `[`geom_polygon`](https://ggplot2.tidyverse.org/reference/geom_polygon.html)`(``data ``=`` ``map_base``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``group ``=`` ``group``)``, `\
`               colour ``=`` ``NA``, fill ``=`` ``"grey80"``)`` ``+`\
`  `[`coord_fixed`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)`(``ratio ``=`` ``1``, xlim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``13.0``, ``23.0``)``, ylim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``-``33``, ``-``42``)``, expand ``=`` ``TRUE``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``""``, y ``=`` ``""``)`` ``+`\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"bottom"``)`\
\
`map_both`` ``<-`` ``ggpubr``::`[`ggarrange`](https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html)`(``map_slope``, ``map_p``, align ``=`` ``"hv"``)`\
`map_both`

From the figure above we may see that the entire study area shows
significant (*p*\<= 0.05) increases in the count of MHWs per year. This
is generally the case for the entire globe. Not shown here is the
significant increase in the intensity of MHWs as well.
