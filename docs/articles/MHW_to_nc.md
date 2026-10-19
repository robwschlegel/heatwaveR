# Saving MHW Results to NetCDF

## Overview

In the previous vignette we saw how to [detect marine heatwaves (MHWs)
in gridded
data](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html).
In this vignette we will use those gridded MHW results to see how to
save them as a NetCDF file.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`` ``# For basic data manipulation`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ncdf4`](https://cirrus.ucsd.edu/~pierce/ncdf/)`)`` ``# For creating NetCDF files`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidync`](https://docs.ropensci.org/tidync/)`)`` ``# For easily dealing with NetCDF data`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`` ``# For visualising data`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`doParallel`](https://github.com/RevolutionAnalytics/doparallel)`)`` ``# For parallel processing`

## Loading data

Please see the [downloading and preparing OISST
data](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html)
and [detecting marine heatwaves (MHWs) in gridded
data](https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html)
vignettes first if you have not yet worked through them. We will be
using the MHW results created from those two vignettes below.

\
`MHW_res_grid`` ``<-`` `[`readRDS`](https://rdrr.io/r/base/readRDS.html)`(``"~/Desktop/MHW_result.Rds"``)`

## Prepare the data

The main sticking point between MHW results and the NetCDF file format
in R is that NetCDF files will only accept the file type in R known as
“arrays”, and most R outputs are data.frames. So the majority of what we
need to do is to convert our data from data.frames to arrays. There are
many ways to do this and a search of the interwebs will produce a
plethora of results. Over the years I have settled into an approach that
I use operationally for the MHW Tracker that I will also use here. It is
not necessarily the fastest method, nor does it use the fewest lines of
code possible, but it does follow the **`tidyverse`** approach to
programming, and I think it is about as transparent as this process can
be. We will create two functions below that will help us along in this
process.

\
`# Function for creating arrays from data.frames`\
`df_acast`` ``<-`` ``function``(``df``, ``lon_lat``)``{`\
`  `\
`  ``# Force grid`\
`  ``res`` ``<-`` ``df`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`right_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``lon_lat``, by ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"lon"``, ``"lat"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`    `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``lon``, ``lat``)`\
`  `\
`  ``# Convert date values to integers if they are present`\
`  ``if``(``lubridate``::`[`is.Date`](https://lubridate.tidyverse.org/reference/date_utils.html)`(``res``[``1``,``4``]``)``)`` ``res``[``,``4``]`` ``<-`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``res``[``,``4``]``)`\
`  `\
`  ``# Create array`\
`  ``res_array`` ``<-`` ``base``::`[`array`](https://rdrr.io/r/base/array.html)`(``res``[``,``4``]``, dim ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``lon_lat``$``lon``)``)``, `[`length`](https://rdrr.io/r/base/length.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``lon_lat``$``lat``)``)``)``)`\
`  `[`dimnames`](https://rdrr.io/r/base/dimnames.html)`(``res_array``)`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``lon ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``lon_lat``$``lon``)``,`\
`                              lat ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``lon_lat``$``lat``)``)`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(``res_array``)`\
`}`\
\
`# Wrapper function for last step before data are entered into NetCDF files`\
`df_proc`` ``<-`` ``function``(``df``, ``col_choice``)``{`\
`  `\
`  ``# Determine the correct array dimensions`\
`  ``lon_step`` ``<-`` `[`mean`](https://rdrr.io/r/base/mean.html)`(`[`diff`](https://rdrr.io/r/base/diff.html)`(`[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``df``$``lon``)``)``)``)`\
`  ``lat_step`` ``<-`` `[`mean`](https://rdrr.io/r/base/mean.html)`(`[`diff`](https://rdrr.io/r/base/diff.html)`(`[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``df``$``lat``)``)``)``)`\
`  ``lon`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`min`](https://rdrr.io/r/base/Extremes.html)`(``df``$``lon``)``, `[`max`](https://rdrr.io/r/base/Extremes.html)`(``df``$``lon``)``, by ``=`` ``lon_step``)`\
`  ``lat`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`min`](https://rdrr.io/r/base/Extremes.html)`(``df``$``lat``)``, `[`max`](https://rdrr.io/r/base/Extremes.html)`(``df``$``lat``)``, by ``=`` ``lat_step``)`\
`  `\
`  ``# Create full lon/lat grid`\
`  ``lon_lat`` ``<-`` `[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)`(``lon ``=`` ``lon``, lat ``=`` ``lat``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``)`\
`  `\
`  ``# Acast only the desired column`\
`  ``dfa`` ``<-`` ``plyr``::`[`daply`](https://rdrr.io/pkg/plyr/man/daply.html)`(``df``[`[`c`](https://rdrr.io/r/base/c.html)`(``"lon"``, ``"lat"``, ``"event_no"``, ``col_choice``)``]``, `\
`                     `[`c`](https://rdrr.io/r/base/c.html)`(``"event_no"``)``, ``df_acast``, .parallel ``=`` ``T``, lon_lat ``=`` ``lon_lat``)`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(``dfa``)`\
`}`\
\
`# We must now run this function on each column of data we want to add to the NetCDF file`\
`doParallel``::`[`registerDoParallel`](https://rdrr.io/pkg/doParallel/man/registerDoParallel.html)`(``cores ``=`` ``7``)`\
`prep_dur`` ``<-`` ``df_proc``(``MHW_res_grid``, ``"duration"``)`\
`prep_max_int`` ``<-`` ``df_proc``(``MHW_res_grid``, ``"intensity_max"``)`\
`prep_cum_int`` ``<-`` ``df_proc``(``MHW_res_grid``, ``"intensity_cumulative"``)`\
`prep_peak`` ``<-`` ``df_proc``(``MHW_res_grid``, ``"date_peak"``)`

## Create NetCDF shell

With our data prepared into a series of arrays, we now need to set the
stage for our NetCDF file. The important thing here is that the
dimensions of the arrays we created match up to the dimensions of the
NetCDF file.

\
`# Get file attributes`\
`lon_step`` ``<-`` `[`mean`](https://rdrr.io/r/base/mean.html)`(`[`diff`](https://rdrr.io/r/base/diff.html)`(`[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``MHW_res_grid``$``lon``)``)``)``)`\
`lat_step`` ``<-`` `[`mean`](https://rdrr.io/r/base/mean.html)`(`[`diff`](https://rdrr.io/r/base/diff.html)`(`[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``MHW_res_grid``$``lat``)``)``)``)`\
`lon`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`min`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``lon``)``, `[`max`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``lon``)``, by ``=`` ``lon_step``)`\
`lat`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`min`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``lat``)``, `[`max`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``lat``)``, by ``=`` ``lat_step``)`\
`event_no`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(`[`min`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``event_no``)``, `[`max`](https://rdrr.io/r/base/Extremes.html)`(``MHW_res_grid``$``event_no``)``, by ``=`` ``1``)`\
`tunits`` ``<-`` ``"days since 1970-01-01"`\
\
`# Length of each attribute`\
`nlon`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``lon``)`\
`nlat`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``lat``)`\
`nen`` ``<-`` `[`length`](https://rdrr.io/r/base/length.html)`(``event_no``)`

## Create NetCDF file

The last step in this process is to add the prepared data to the NetCDF
shell we have constructed. The **`ncdf4`** package helps to make this
all a relatively straight forward process.

\
`# Path and file name`\
`  ``# NB: We are net setting time dimensions here because we are using event_no as our "time" dimension`\
`ncpath`` ``<-`` ``"~/Desktop/"`\
`ncname`` ``<-`` ``"MHW_results"`\
`ncfname`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``ncpath``, ``ncname``, ``".nc"``)`\
`# dname <- "tmp"`\
\
`# Define dimensions`\
`londim`` ``<-`` `[`ncdim_def`](https://rdrr.io/pkg/ncdf4/man/ncdim_def.html)`(``"lon"``, ``"degrees_east"``, ``lon``)`\
`latdim`` ``<-`` `[`ncdim_def`](https://rdrr.io/pkg/ncdf4/man/ncdim_def.html)`(``"lat"``, ``"degrees_north"``, ``lat``)`\
`endim`` ``<-`` `[`ncdim_def`](https://rdrr.io/pkg/ncdf4/man/ncdim_def.html)`(``"event_no"``, ``"event_number"``, ``event_no``)`\
`# timedim <- ncdim_def("time", tunits, as.double(time))`\
\
`# Define variables`\
`fillvalue`` ``<-`` ``1e32`\
`def_dur`` ``<-`` `[`ncvar_def`](https://rdrr.io/pkg/ncdf4/man/ncvar_def.html)`(``name ``=`` ``"duration"``, units ``=`` ``"days"``, dim ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``endim``, ``latdim``, ``londim``)``, `\
`                     missval ``=`` ``fillvalue``, longname ``=`` ``"duration of MHW"``, prec ``=`` ``"double"``)`\
`def_max_int`` ``<-`` `[`ncvar_def`](https://rdrr.io/pkg/ncdf4/man/ncvar_def.html)`(``name ``=`` ``"max_int"``, units ``=`` ``"deg_c"``, dim ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``endim``, ``latdim``, ``londim``)``, `\
`                         missval ``=`` ``fillvalue``, longname ``=`` ``"maximum intensity during MHW"``, prec ``=`` ``"double"``)`\
`def_cum_int`` ``<-`` `[`ncvar_def`](https://rdrr.io/pkg/ncdf4/man/ncvar_def.html)`(``name ``=`` ``"cum_int"``, units ``=`` ``"deg_c days"``, dim ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``endim``, ``latdim``, ``londim``)``, `\
`                         missval ``=`` ``fillvalue``, longname ``=`` ``"cumulative intensity during MHW"``, prec ``=`` ``"double"``)`\
`def_peak`` ``<-`` `[`ncvar_def`](https://rdrr.io/pkg/ncdf4/man/ncvar_def.html)`(``name ``=`` ``"date_peak"``, units ``=`` ``tunits``, dim ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``endim``, ``latdim``, ``londim``)``,`\
`                      missval ``=`` ``0``, longname ``=`` ``"date of peak temperature anomaly during MHW"``, prec ``=`` ``"integer"``)`\
\
`# Create netCDF file and prepare space for our arrays`\
`ncout`` ``<-`` `[`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html)`(``ncfname``, `[`list`](https://rdrr.io/r/base/list.html)`(``def_peak``, ``def_dur``, ``def_max_int``, ``def_cum_int``)``, force_v4 ``=`` ``TRUE``)`\
\
`# Add the actual data`\
[`ncvar_put`](https://rdrr.io/pkg/ncdf4/man/ancvar_put.html)`(``ncout``, ``def_dur``, ``prep_dur``)`\
[`ncvar_put`](https://rdrr.io/pkg/ncdf4/man/ancvar_put.html)`(``ncout``, ``def_max_int``, ``prep_max_int``)`\
[`ncvar_put`](https://rdrr.io/pkg/ncdf4/man/ancvar_put.html)`(``ncout``, ``def_cum_int``, ``prep_cum_int``)`\
[`ncvar_put`](https://rdrr.io/pkg/ncdf4/man/ancvar_put.html)`(``ncout``, ``def_peak``, ``prep_peak``)`\
\
`# Put additional attributes into dimension and data variables`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``"lon"``, ``"axis"``, ``"X"``)`` ``#,verbose=FALSE) #,definemode=FALSE)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``"lat"``, ``"axis"``, ``"Y"``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``"event_no"``, ``"axis"``, ``"event_no"``)`\
\
`# Add global attributes`\
`  ``# These are useful for whomever else may want to use your NetCDF file`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``0``, ``"title"``, `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"MHW results from lon: "``,`\
`                                    `[`min`](https://rdrr.io/r/base/Extremes.html)`(``lon``)``,``" to "``,`[`max`](https://rdrr.io/r/base/Extremes.html)`(``lon``)``,`\
`                                    ``" and lat: "``,`[`min`](https://rdrr.io/r/base/Extremes.html)`(``lat``)``,``" to "``,`[`max`](https://rdrr.io/r/base/Extremes.html)`(``lat``)``)``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``0``, ``"institution"``, ``"Your institution here!"``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``0``, ``"source"``, ``"NOAA OISST v2.1"``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``,``0``, ``"references"``, ``"Banzon et al. (2020) J. Atmos. Oce. Tech. 37:341-349"``)`\
`history`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"Your name here!, "``, `[`date`](https://lubridate.tidyverse.org/reference/date.html)`(``)``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``0``, ``"history"``, ``history``)`\
[`ncatt_put`](https://rdrr.io/pkg/ncdf4/man/ncatt_put.html)`(``ncout``, ``0``, ``"Conventions"``, ``"Hobday et al. (2016)"``)`` ``# Assuming one has used the Hobday definition`\
\
`# Get a summary of the created file:`\
`ncout`

## Visualising the results

Let’s not stop there. To ensure that the NetCDF file was created
correctly we want to load it back into our workspace and plot the
results.

\
`# Convenience function for comparing files`\
`quick_grid`` ``<-`` ``function``(``df``, ``var_choice``)``{`\
`  ``df`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``event_no`` ``==`` ``13``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`    `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``lon``, y ``=`` ``lat``)``)`` ``+`\
`    `[`geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(`[`aes_string`](https://ggplot2.tidyverse.org/reference/aes_.html)`(``fill ``=`` ``var_choice``)``)`` ``+`\
`    `[`coord_cartesian`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)`(``expand ``=`` ``F``)`` ``+`\
`    `[`scale_fill_viridis_c`](https://ggplot2.tidyverse.org/reference/scale_viridis.html)`(``)`` ``+`\
`    `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``legend.position ``=`` ``"bottom"``)`\
`}`\
\
`# Thanks to the tidync package, loading the gridded data is very simple`\
`MHW_res_nc`` ``<-`` `[`tidync`](https://docs.ropensci.org/tidync/reference/tidync.html)`(``"~/Desktop/MHW_results.nc"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`hyper_tibble`](https://docs.ropensci.org/tidync/reference/hyper_tibble.html)`(``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``date_peak ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``date_peak``, origin ``=`` ``"1970-01-01"``)``)`\
\
`# Plot the duration results`\
`quick_grid``(``MHW_res_grid``, ``"duration"``)`\
`quick_grid``(``MHW_res_nc``, ``"duration"``)`\
\
`# Cumulative intensity`\
`quick_grid``(``MHW_res_grid``, ``"intensity_cumulative"``)`\
`quick_grid``(``MHW_res_nc``, ``"cum_int"``)`
