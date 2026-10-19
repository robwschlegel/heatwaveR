# Trend and Breakpoint analyses in MHW metrics

## Overview

One may see in other vignettes how to [download and prepare OISST
data](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html)
and how to [detect MHWs in gridded
data](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html).
In this vignette we will use data subsetted around the New Zealand (NZ)
Exclusive Economic Zone (EEZ) for our example to calculate trends and
breakpoints for bioregions (from Spalding et al. (2007)) and seasons as
shown in Thoral et al. (2022).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`heatwaveR`](https://robwschlegel.github.io/heatwaveR/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`lubridate`](https://lubridate.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyr`](https://tidyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`purrr`](https://purrr.tidyverse.org/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`viridis`](https://sjmgarnier.github.io/viridis/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``trend``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`kableExtra`](https://haozhu233.github.io/kableExtra/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`rerddap`](https://docs.ropensci.org/rerddap/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`sf`](https://r-spatial.github.io/sf/)`)`

## MEOW

The Marine Ecosystems of the World (MEOW) shapefile may be downloaded
[here](https://hub.arcgis.com/datasets/903c3ae05b264c00a3b5e58a4561b7e6/about).
Extract the contents of the download to a convenient location before use
with the following code chunks. For more information on the MEOW please
see Spalding et al. (2007).

\
`# Load and process the MEOW shapefile`\
`# NB: Change your file path to where the files were unzipped`\
`MEOW_NZ`` ``<-`` `[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html)`(``'~/Desktop/meow_ecos.shp'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``:::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``PROVINCE`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``'Northern New Zealand'``, ``'Southern New Zealand'``,`\
`                                 ``'Subantarctic New Zealand'``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`st_make_valid`](https://r-spatial.github.io/sf/reference/valid.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`st_transform`](https://r-spatial.github.io/sf/reference/st_transform.html)`(``crs ``=`` ``4326``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`st_shift_longitude`](https://r-spatial.github.io/sf/reference/st_shift_longitude.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ECOREGION ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``ECOREGION``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Kermadec Island"``, ``"Three Kings-North Cape"``, `\
`                                                  ``"Northeastern New Zealand"``, ``"Central New Zealand"``,`\
`                                                  ``"Chatham Island"``, ``"South New Zealand"``, `\
`                                                  ``"Bounty and Antipodes Islands"``,`\
`                                                  ``"Snares Island"``,``"Auckland Island"``,``"Campbell Island"``)``)``)`\
\
`# Find max lon/lat ranges`\
`lon_range`` ``<-`` `[`range`](https://rdrr.io/r/base/range.html)`(``sf``::`[`st_coordinates`](https://r-spatial.github.io/sf/reference/st_coordinates.html)`(``MEOW_NZ``$``geometry``)``[``,``1``]``)`\
`lat_range`` ``<-`` `[`range`](https://rdrr.io/r/base/range.html)`(``sf``::`[`st_coordinates`](https://r-spatial.github.io/sf/reference/st_coordinates.html)`(``MEOW_NZ``$``geometry``)``[``,``2``]``)`\
\
`# Expand a bit to make sure all necessary pixels are downloaded`\
`lon_range`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``lon_range``[``1``]``-``0.25``, ``lon_range``[``2``]``+``0.25``)`\
`lat_range`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``lat_range``[``1``]``-``0.25``, ``lat_range``[``2``]``+``0.25``)`

![MEOW bioregions around New Zealand](https://i.imgur.com/FVEOCNP.png)

MEOW bioregions around New Zealand

Once we have our MEOW data for NZ ready it’s time to find out which
pixels specifically we will want to analyse. The process of assigning
bioregions to pixels is simplified by using the `sf` (Pebesma (2018))
package.

\
`# Create OISST grid`\
`lon_lat_OISST`` ``<-`` ``base``::`[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(`[`seq`](https://rdrr.io/r/base/seq.html)`(``0.125``, ``359.875``, by ``=`` ``0.25``)``, `\
`                                   `[`seq`](https://rdrr.io/r/base/seq.html)`(``-``89.875``, ``89.875``, by ``=`` ``0.25``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``lon ``=`` ``Var1``, lat ``=`` ``Var2``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`st_as_sf`](https://r-spatial.github.io/sf/reference/st_as_sf.html)`(``coords ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``'lon'``, ``'lat'``)``, crs ``=`` ``4326``)`\
\
`# Join OISST grid to MEOW spatial and filter out pixels not within NZ EEZ`\
`lon_lat_NZ`` ``<-`` `[`st_join`](https://r-spatial.github.io/sf/reference/st_join.html)`(``lon_lat_OISST``, ``MEOW_NZ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``geometry``, ``ECOREGION``, ``PROVINCE``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``ECOREGION``)``)`\
\
`# Convert the coordinates back to a tibble for further use`\
`# NB: Should be 6,398 pixels`\
`lon_lat_NZ_df`` ``<-`` ``lon_lat_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``lon ``=`` ``sf``::`[`st_coordinates`](https://r-spatial.github.io/sf/reference/st_coordinates.html)`(``.``)``[``,``1``]``,`\
`         lat ``=`` ``sf``::`[`st_coordinates`](https://r-spatial.github.io/sf/reference/st_coordinates.html)`(``.``)``[``,``2``]``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``lon ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``lon``)``, lat ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(``lat``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``geometry``)`

## Download data and calculate climatologies

We begin by downloading daily OISST v2.1 data for all pixels around New
Zealand (lat 25-55°S, lon 160-190°E) and use the functions
`heatwaveR::ts2clim()` and
[`heatwaveR::detect_event()`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
to get the climatology from the time series. Note that this full process
will take roughly one hour on a desktop computer given the number of
pixels and days. Even though we know the exact coordinates of the pixels
that we want, it is still faster to use the
[`rerddap::griddap()`](https://docs.ropensci.org/rerddap/reference/griddap.html)
function to extract a bounding box around NZ rather than to use the
functions in the **`raster`** package to extract the EEZ polygons.

\
`# Get SST for around NZ + Islands and for all years`\
`OISST_sub_dl`` ``<-`` ``function``(``time_df``)``{`\
`  ``OISST_dat`` ``<-`` `[`griddap`](https://docs.ropensci.org/rerddap/reference/griddap.html)`(``x ``=`` ``"ncdcOisst21Agg"``, `\
`                       url ``=`` ``"https://coastwatch.pfeg.noaa.gov/erddap/"``, `\
`                       time ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``time_df``$``start``, ``time_df``$``end``)``, `\
`                       zlev ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``0``)``,`\
`                       latitude ``=`` ``lat_range``,`\
`                       longitude ``=`` ``lon_range``,`\
`                       fields ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"sst"``)``)``$``data`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``time ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``stringr``::`[`str_remove`](https://stringr.tidyverse.org/reference/str_remove.html)`(``time``, ``"T00:00:00Z"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``t ``=`` ``time``, temp ``=`` ``sst``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``lon``, ``lat``, ``t``, ``temp``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`na.omit`](https://rdrr.io/r/stats/na.fail.html)`(``)`\
`}`\
\
`# The span of years to download`\
`dl_years`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``date_index ``=`` ``1``:``5``,`\
`                       start ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"1990-01-01"``,`\
`                                         ``"1998-01-01"``, ``"2006-01-01"``, ``"2014-01-01"``)``)``,`\
`                       end ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"1989-12-31"``, ``"1997-12-31"``,`\
`                                       ``"2005-12-31"``, ``"2013-12-31"``, ``"2021-12-31"``)``)``)`\
\
`# Download the data`\
`# NB: Takes ~21 minutes on a desktop computer`\
`# NB: Contains 175,208,352 rows, 4 columns, and uses ~15 GB of RAM`\
`OISST_data`` ``<-`` ``dl_years`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``date_index``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~``OISST_sub_dl``(``.x``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``lon``, ``lat``, ``t``, ``temp``)`\
\
`# Filter out only the NZ pixels`\
`# NB: Contains 87,195,152 rows`\
`OISST_data_NZ`` ``<-`` `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``lon_lat_NZ_df``, ``OISST_data``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"lon"``, ``"lat"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`drop_na`](https://tidyr.tidyverse.org/reference/drop_na.html)`(``)`\
`  `\
`# Get the climatologies only`\
`clim_only`` ``<-`` ``function``(``df``)``{`\
`  ``# First calculate the climatologies`\
`  ``clim`` ``<-`` `[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``data ``=`` ``df``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)`\
`  ``# Then the events`\
`  ``event`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(``data ``=`` ``clim``)`\
`  ``# Return only the climatology metric dataframe of results`\
[`return`](https://rdrr.io/r/base/function.html)`(``event``$``climatology``)`\
`}`\
\
`# Extract the climatology values`\
`# NB: Takes ~21 minutes on a desktop computer`\
`OISST_clim`` ``<-`` ``OISST_data_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``ECOREGION``, ``PROVINCE``, ``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~``clim_only``(``.x``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`drop_na`](https://tidyr.tidyverse.org/reference/drop_na.html)`(``)`

## Summarise MHW metrics by year, season, and bioregion

\
`# Get summary`\
`MHW_summary`` ``<-`` ``OISST_clim`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-`[`c`](https://rdrr.io/r/base/c.html)`(``doy``, ``thresh``, ``threshCriterion``, ``durationCriterion``, ``event``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``season_num ``=`` `[`month`](https://lubridate.tidyverse.org/reference/month.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`floor_date`](https://lubridate.tidyverse.org/reference/round_date.html)`(``t``, unit ``=`` ``"season"``)``)``)``,`\
`         year ``=`` `[`year`](https://lubridate.tidyverse.org/reference/year.html)`(``t``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``season ``=`` `[`recode_factor`](https://dplyr.tidyverse.org/reference/recode.html)`(``season_num``` , `12`  ```=`` ``"Summer"``` , `3`  ```=`` ``"Autumn"``, `\
``                                 `6`  ```=`` ``"Winter"``` , `9`  ```=`` ``"Spring"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``intensity ``=`` ``temp``-``seas``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``ECOREGION``, ``PROVINCE``, ``lon``, ``lat``, ``year``, ``season``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(``nevents ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``event_no``)``)``,`\
`            nMHWdays ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(``t``)``,`\
`            int_cumulative ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``intensity``)``, `\
`            int_mean ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``intensity``)``,`\
`            int_max ``=`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``intensity``)``, .groups ``=`` ``"drop"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``-`[`c`](https://rdrr.io/r/base/c.html)`(``ECOREGION``, ``PROVINCE``, ``lon``, ``lat``, ``year``, ``season``)``, `\
`               names_to ``=`` ``'metrics'``, values_to ``=`` ``'values'``)`\
\
`# Save for future use`\
`# NB: 10.8 MB`\
[`saveRDS`](https://rdrr.io/r/base/readRDS.html)`(``MHW_summary``,``"~/Desktop/MHW_summary_NZ_1982_2021_OISST.Rds"``)`

## Trends in MHW metrics at NZ scale + Bioregions

\
`# Load data from previous code chunk`\
`MHW_summary_NZ`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(``"~/Desktop/MHW_summary_NZ_1982_2021_OISST.Rds"``)`\
\
`# Get count of unique pixels`\
`MHW_summary_NZ_npix`` ``<-`` ``MHW_summary_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``lon``, ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`distinct`](https://dplyr.tidyverse.org/reference/distinct.html)`(``)`\
\
`# Get annual summaries`\
`MHW_annual_summary_NZ`` ``<-`` ``MHW_summary_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)`(``names_from ``=`` ``metrics``, values_from ``=`` ``values``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`summarize`](https://dplyr.tidyverse.org/reference/summarise.html)`(``Number_MHW_days ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``nMHWdays``)``/`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``MHW_summary_NZ_npix``)``,`\
`            Nevents ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``nevents``)``/`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``MHW_summary_NZ_npix``)``, `\
`            Mean_Intensity ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``int_mean``)``, `\
`            Maximum_Intensity ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``int_max``)``,`\
`            Cumulative_Intensity ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``int_cumulative``)``/`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``MHW_summary_NZ_npix``)``, .groups ``=`` ``"drop"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`complete`](https://tidyr.tidyverse.org/reference/complete.html)`(``year ``=`` ``1982``:``2021``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``-``year``, names_to ``=`` ``'Metrics'``, values_to ``=`` ``'values'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Metrics ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``Metrics``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Number_MHW_days"``, ``"Nevents"``, ``"Mean_Intensity"``, `\
`                                              ``"Maximum_Intensity"``, ``"Cumulative_Intensity"``)``)``)`\
\
`# Prepare prettier labels`\
`metrics_labs`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``` `Number_MHW_days`  ```=`` ``"Number of MHW days"``,`\
``                   `Nevents`  ```=`` ``"Number of Events"``,`\
``                   `Mean_Intensity`  ```=`` ``"Mean Intensity (°C)"``,`\
``                   `Maximum_Intensity`  ```=`` ``"Maximum Intensity (°C)"``,`\
``                   `Cumulative_Intensity`  ```=`` ``"Cumulative Intensity (°C Days)"``)`\
\
`# Create figure`\
`p`` ``<-`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``MHW_annual_summary_NZ``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``year``, y ``=`` ``values``, col ``=`` ``Metrics``)``)`` ``+`` `\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``size ``=`` ``0.5``)`` ``+`` `\
`  `[`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)`(``se ``=`` ``T``)`` ``+`\
`  ``scale_colour_viridis``(``begin ``=`` ``0``, end ``=`` ``0.75``, option ``=`` ``"viridis"``, discrete ``=`` ``T``)`` ``+`` `\
`  `[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)`(``Metrics`` ``~`` ``.``, scales ``=`` ``'free'``, labeller ``=`` `[`as_labeller`](https://ggplot2.tidyverse.org/reference/as_labeller.html)`(``metrics_labs``)``, ncol ``=`` ``2``)`` ``+`` `\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Year"``, Y ``=`` ``NULL``)`` ``+`\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"none"``)`` `\
`p`\
\
`# Coerce to interactive plotly format`\
`# plotly::ggplotly(p)`

It seems like some of the metrics are going up, and that there is a
potential acceleration after the years 2010. We can then calculate non
parametric linear trends (Sen’s slope and Mann-Kendall test) and
breakpoints (Pettitt test) using the `trend` (Pohlert (2020)) package as
well as the more usual and parametric `lm` function.

\
`# Get annual trends`\
`MHW_annual_trends_NZ`` ``<-`` ``MHW_annual_summary_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``Metrics``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`nest`](https://tidyr.tidyverse.org/reference/nest.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ts_out ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``, ``~`[`ts`](https://rdrr.io/r/stats/ts.html)`(``.x``$``values``, start ``=`` ``1982``, end ``=`` ``2021``, frequency ``=`` ``1``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``, `\
`         pettitt ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``pettitt.test``(``.x``)``)``,`\
`         lm ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``, ``~`[`lm`](https://rdrr.io/r/stats/lm.html)`(``values`` ``~`` ``year``, ``.x``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Sens_Slope ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``1``]``)``,`\
`         P_Value ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``3``]``)``,`\
`         Change_Point_Year ``=`` `[`time`](https://rdrr.io/r/stats/time.html)`(``ts_out``[[``1``]``]``)``[`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``3``]``)``]``,`\
`         Change_Point_pvalue ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``4``]``)``,`\
`         lm_slope ``=`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``lm``)``$``coefficients.year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Add step of cutting time series in 2 using Change_Point_Year `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``pre_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``1982``, end ``=`` ``Change_Point_Year``)``)``,`\
`         post_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``Change_Point_Year``, end ``=`` ``2021``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Add step of calculating sen's slope and p-value to pre and post change point year`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens_pre ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``pre_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``1``]``)``, P_Value_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``3``]``)``,`\
`         sens_post ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``post_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``1``]``)``,`\
`         P_Value_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``3``]``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``Metrics``, ``Sens_Slope``, ``P_Value``, ``Change_Point_Year``, ``Change_Point_pvalue``, ``lm_slope``,`\
`                ``Sens_Slope_pre``, ``P_Value_pre``, ``Sens_Slope_post``, ``P_Value_post``)`\
\
`# Create table`\
`kable``(``MHW_annual_trends_NZ``, caption ``=`` ``"Table 1 - Sens's Slope and p-value."``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``kable_classic``(``)`

In fact, we see significant change-point years for the number of MHW
days (2012), number of events (1997) and cumulative intensity (2012).
The slopes pre and post change-point are documenting the recent change
in the metrics which complement the trend on the full time series. What
are the reason behind the change-points?

One can be more interested in finding patterns in MHW trends between
ecoregions.

\
`# Count pixels per realm`\
`MHW_summary_NZ_npix_realms`` ``<-`` ``MHW_summary_NZ`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``lon``, ``lat``, ``ECOREGION``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`tally`](https://dplyr.tidyverse.org/reference/count.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``ECOREGION``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`tally`](https://dplyr.tidyverse.org/reference/count.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``npix ``=`` ``n``)`\
`MHW_summary_NZ_npix_realms`\
\
`# Summarise by realm`\
`MHW_summary_NZ_realms`` ``<-`` `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``MHW_summary_NZ``, ``MHW_summary_NZ_npix_realms``, by ``=`` ``'ECOREGION'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)`(``names_from ``=`` ``metrics``,values_from ``=`` ``values``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``year``, ``ECOREGION``, ``season``, ``npix``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`summarize`](https://dplyr.tidyverse.org/reference/summarise.html)`(``Number_MHW_days ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``nMHWdays``)``,`\
`            Nevents ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``nevents``)``, `\
`            Mean_Intensity ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``int_mean``)``, `\
`            Maximum_Intensity ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``int_max``)``,`\
`            Cumulative_Intensity ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``int_cumulative``)``, .groups ``=`` ``"drop"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Number_MHW_days ``=`` ``Number_MHW_days``/``npix``,`\
`         Nevents ``=`` ``Nevents``/``npix``, `\
`         Cumulative_Intensity ``=`` ``Cumulative_Intensity``/``npix``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``ECOREGION``, ``season``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`complete`](https://tidyr.tidyverse.org/reference/complete.html)`(``year ``=`` ``1982``:``2021``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``npix``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``-`[`c`](https://rdrr.io/r/base/c.html)`(``year``, ``ECOREGION``, ``season``)``, names_to ``=`` ``'Metrics'``, values_to ``=`` ``'values'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Metrics ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``Metrics``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Number_MHW_days"``, ``"Nevents"``, ``"Mean_Intensity"``, `\
`                                              ``"Maximum_Intensity"``, ``"Cumulative_Intensity"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`replace`](https://rdrr.io/r/base/replace.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.``)``, ``0``)`\
\
`# Plot the results`\
`p`` ``<-`` ``MHW_summary_NZ_realms`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``Metrics`` ``==`` ``'Number_MHW_days'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``year``, y ``=`` ``values``, colour ``=`` ``season``)``)`` ``+`` `\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``size ``=`` ``0.5``)`` ``+`` `\
`  `[`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)`(``se ``=`` ``T``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Year"``, y ``=`` ``"Number of MHW days"``)`` ``+`\
`  ``scale_colour_viridis``(``begin ``=`` ``0``, end ``=`` ``0.75``, option ``=`` ``"inferno"``, discrete ``=`` ``T``)`` ``+`\
`  `[`guides`](https://ggplot2.tidyverse.org/reference/guides.html)`(``colour ``=`` `[`guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html)`(``override.aes ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``shape ``=`` ``15``, size ``=`` ``2``)``)``)`` ``+`\
`  `[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)`(``ECOREGION``~``.``, ncol ``=`` ``3``)`` ``+`` `\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.8``,``0.05``)``,`\
`        legend.title ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``)`\
`p`\
\
`# Coerce to plotly format`\
`# plotly::ggplotly(p)`

We could also show similar graphs for other metrics, like number of
events, min, max or cumulative intensity.

\
`# Get trends`\
`MHW_trends_NZ_realms`` ``<-`` ``MHW_summary_NZ_realms`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``Metrics``, ``ECOREGION``, ``season``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`nest`](https://tidyr.tidyverse.org/reference/nest.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ts_out ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``, ``~`[`ts`](https://rdrr.io/r/stats/ts.html)`(``.x``$``values``, start ``=`` ``1982``, end ``=`` ``2021``, frequency ``=`` ``1``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         pettitt ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``pettitt.test``(``.x``)``)``,`\
`         lm ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``, ``~`[`lm`](https://rdrr.io/r/stats/lm.html)`(``values`` ``~`` ``year``,``.x``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Sens_Slope ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``1``]``)``,`\
`         P_Value ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``3``]``)``,`\
`         Change_Point_Year ``=`` `[`time`](https://rdrr.io/r/stats/time.html)`(``ts_out``[[``1``]``]``)``[`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``3``]``)``]``,`\
`         Change_Point_pvalue ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``4``]``)``,`\
`         lm_slope ``=`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``lm``)``$``coefficients.year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Add step of cutting time series in 2 using Change_Point_Year `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``pre_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``1982``, end ``=`` ``Change_Point_Year``)``)``,`\
`         post_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``Change_Point_Year``, end ``=`` ``2021``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Add step of calculating sen's slope and p-value to pre and post change point year`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens_pre ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``pre_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``1``]``)``,`\
`         P_Value_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``3``]``)``,`\
`         sens_post ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``post_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``1``]``)``,`\
`         P_Value_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``3``]``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` ``# To remove Season column`\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``ECOREGION``, ``Metrics``, ``season``, ``Sens_Slope``, ``P_Value``, ``Change_Point_Year``, ``Change_Point_pvalue``,`\
`                ``lm_slope``, ``Sens_Slope_pre``, ``P_Value_pre``, ``Sens_Slope_post``, ``P_Value_post``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``Metrics`` ``==`` ``'Number_MHW_days'``)`\
\
`# Create table`\
`kable``(``MHW_trends_NZ_realms``, caption ``=`` ``"Table 2 - Sens's Slope and p-value."``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``kable_classic``(``)`

According to the previous tables, the trend and break point analyses are
useful to asses changes in MHW metrics at these scales. But MHWs are by
definition discrete events in space and time. So some can argue that
having a pixel and event-based approach is preferable. Can we still
apply this methodology on pixel scale?

## Pixel-based case scenario 

We use the time series of SST available within the `heatwaveR` package
(sst_WA) and investigate the trend analysis in 5 metrics by grouping
metric values per year as we have done above.

\
`# Simple event detection`\
`MHW_WA`` ``<-`` `[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)`(`[`ts2clm`](https://robwschlegel.github.io/heatwaveR/reference/ts2clm.md)`(``sst_WA``, climatologyPeriod ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1982-01-01"``, ``"2011-12-31"``)``)``)`\
\
`# Get annual metrics`\
`MHW_WA_metrics_year`` ``<-`` ``MHW_WA``$``climatology`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``event_no``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``year ``=`` `[`year`](https://lubridate.tidyverse.org/reference/year.html)`(``t``)``,`\
`         intensity ``=`` ``temp``-``seas``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``event``, ``year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html)`(``nevents ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``event_no``)``)``,`\
`            nMHWdays ``=`` `[`length`](https://rdrr.io/r/base/length.html)`(``t``)``,`\
`            int_cumulative ``=`` `[`sum`](https://rdrr.io/r/base/sum.html)`(``intensity``)``, `\
`            int_mean ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``intensity``)``,`\
`            int_max ``=`` `[`max`](https://rdrr.io/r/base/Extremes.html)`(``intensity``)``, .groups ``=`` ``"drop"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Need to complete time series in case of years with no MHWs in order to get full ts in trend analysis`\
`  `[`complete`](https://tidyr.tidyverse.org/reference/complete.html)`(``year ``=`` ``1982``:``2018``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``event``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``-`[`c`](https://rdrr.io/r/base/c.html)`(``year``)``, names_to ``=`` ``'Metrics'``, values_to ``=`` ``'values'``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`replace`](https://rdrr.io/r/base/replace.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``.``)``, ``0``)`\
\
`# Plot the data`\
`p`` ``<-`` `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``MHW_WA_metrics_year``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``year``, y ``=`` ``values``)``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``)`` ``+`\
`  `[`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)`(``se ``=`` ``T``, method ``=`` ``'lm'``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Year"``)`` ``+`` `\
`  `[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)`(``~``Metrics``, scales ``=`` ``'free'``)`` ``+`\
`  `[`theme_bw`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`` `\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"none"``)`` `\
`p`\
\
`# Coerce to plotly`\
`# plotly::ggplotly(p)`

The `lm` regression seems to detect some upward trends in the metrics.
What does Mann-Kendall have to say about these?

\
`MHW_WA_trends`` ``<-`` ``MHW_WA_metrics_year`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``Metrics``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`nest`](https://tidyr.tidyverse.org/reference/nest.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``ts_out ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``, ``~`[`ts`](https://rdrr.io/r/stats/ts.html)`(``.x``$``values``, start ``=`` ``1982``, end ``=`` ``2018``, frequency ``=`` ``1``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``, `\
`         pettitt ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``, ``~``pettitt.test``(``.x``)``)``,`\
`         lm ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``data``,``~`[`lm`](https://rdrr.io/r/stats/lm.html)`(``values`` ``~`` ``year``, ``.x``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``Sens_Slope ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``1``]``)``,`\
`         P_Value ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens``)``[``3``]``)``,`\
`         Change_Point_Year ``=`` `[`time`](https://rdrr.io/r/stats/time.html)`(``ts_out``[[``1``]``]``)``[`[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``3``]``)``]``,`\
`         Change_Point_pvalue ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``pettitt``)``[``4``]``)``,`\
`         lm_slope ``=`` `[`unlist`](https://rdrr.io/r/base/unlist.html)`(``lm``)``$``coefficients.year``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``# Add step of cutting time series in 2 using Change_Point_Year `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``pre_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``,``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``1982``, end ``=`` ``Change_Point_Year``)``)``,`\
`         post_ts ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``ts_out``,``~`[`window`](https://rdrr.io/r/stats/window.html)`(``.x``, start ``=`` ``Change_Point_Year``, end ``=`` ``2018``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``# Add step of calculating sen's slope and p-value to pre and post change point year`\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``sens_pre ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``pre_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``1``]``)``,`\
`         P_Value_pre ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_pre``)``[``3``]``)``,`\
`         sens_post ``=`` ``purrr``::`[`map`](https://purrr.tidyverse.org/reference/map.html)`(``post_ts``, ``~``sens.slope``(``.x``, conf.level ``=`` ``0.95``)``)``,`\
`         Sens_Slope_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``1``]``)``,`\
`         P_Value_post ``=`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(``sens_post``)``[``3``]``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``Metrics``, ``Sens_Slope``, ``P_Value``, ``Change_Point_Year``, ``Change_Point_pvalue``, `\
`                ``lm_slope``, ``Sens_Slope_pre``, ``P_Value_pre``, ``Sens_Slope_post``, ``P_Value_post``)`\
\
`# Create table`\
`kable``(``MHW_WA_trends``, caption ``=`` ``"Table 3 - Sens's Slope and p-value."``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``kable_classic``(``)`

For some reason, the Mann-Kendall and Sen Slope analyses don’t seem to
be useful here as they return suspiciously too low trends (vs lm_slope,
which is the traditional linear regression using `lm`) and quite high
p-values

Summarizing MHW metrics by year in a given pixel will likely result in
some years with no MHWs, hence bringing some 0 values into the time
series. I am not too sure how sensitive the MK and Pettitt (breakpoint)
analyses are to 0 values, something to look into in the future.

## References

Pebesma, Edzer. 2018. “Simple Features for R: Standardized Support for
Spatial Vector Data.” *The R Journal* 10 (1): 439–46.
<https://doi.org/10.32614/RJ-2018-009>.

Pohlert, Thorsten. 2020. *Trend: Non-Parametric Trend Tests and
Change-Point Detection*. <https://CRAN.R-project.org/package=trend>.

Spalding, Mark D, Helen E Fox, Gerald R Allen, et al. 2007. “Marine
Ecoregions of the World: A Bioregionalization of Coastal and Shelf
Areas.” *BioScience* 57 (7): 573–83.

Thoral, François, Shinae Montie, Mads S Thomsen, Leigh W Tait, Matthew H
Pinkerton, and David R Schiel. 2022. “Unravelling Seasonal Trends in
Coastal Marine Heatwave Metrics Across Global Biogeographical Realms.”
*Scientific Reports* 12 (1): 1–13.
