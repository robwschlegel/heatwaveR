# generic_netCDF2csv.R


# NOTES ON USING THIS SCRIPT ----------------------------------------------

# 1. The Reynolds OISST v2 data processed by this script can be retrieved from:
# https://podaac.jpl.nasa.gov/dataset/AVHRR_OI-NCEI-L4-GLOB-v2.0
# 2. Subsetting and the selection of the time steps to be done via the python
# script `subset_dataset.py`.
# 3. This R script requires the already subsetted netCDFs to reside inside a
# directory whose path is specified by `nc.dir`, below.
# 4. The .csv files produced will be placed inside of the directory named by
# `csv.dir` (make sure this directory already exists).
# 5. The dates that will be included with the final .csv file will be extracted
# directly from the names of the daily netCDF files; please, therefore, make
# sure to never change them by manually editing them.
# 6. The base name of the new .csv file will be partly based on the name of the
# input netCDF files, with the start and end dates appended at the end. These
# things are hard coded into the script below.
# 7. I am sure I have missed some things, or that some things may break somehow;
# please let me know if this happens and I shall fix it.
# 8. This file may take a while to run (10s of minutes to hours, depending on
# the amount of data processed); please be patient while it does its thing.

# Author: AJ Smit
# Date: 27 April 2018
# e-mail: ajsmit@uwc.ac.za


# CAUTION -----------------------------------------------------------------

# This function will append data to the end of an existing file that had been
# previously produced by this script. This will result in duplicate data. If you
# need to rerun the script for some reason, please make sure to delete the file
# created as the result of the previous run from the `csv.dir`.


# LOAD LIBRARIES ----------------------------------------------------------

library(ncdf4) # library for processing netCDFs
library(data.table) # for fast .csv write function, `fwrite()`
library(tidyverse) # misc. data processing conveniences
library(reshape2) # for making a long data format
library(plyr) # for `llply()`
library(lubridate) # for working with dates
library(stringr) # for working with strings
library(doMC); doMC::registerDoMC(cores = 4) # for multicore spead-ups


# SPECIFY FILE PATHS ------------------------------------------------------

# Setup OISST netCDF data path and csv file output directory
nc.dir <- "/Users/Rita_MHW_events/netCDF"
csv.dir <- "/Users/Rita_MHW_events/csv"

# PARSE FILE INFO (not used directly) -------------------------------------

# Use to determine the start/end points of the `name.stem` (see code below)
#          1         2         3         4         5         6         7
# 123456789012345678901234567890123456789012345678901234567890123456789012345
# 20091231120000-NCEI-L4_GHRSST-SSTblend-AVHRR_OI-GLOB-v02.0-fv02.0_subset.nc


# OISST netCDF READ FUNCTION ----------------------------------------------

# Function to extract the dims and data from OISST netCDFs
ncRead <- function(nc.dir = nc.dir, csv.dir = csv.dir) {
  nc.list <- list.files(path = nc.dir, pattern = "*.nc", full.names = TRUE, include.dirs = TRUE)
  nc.first <- head(list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE), 1)
  nc.last <- tail(list.files(path = nc.dir, pattern = "*.nc", full.names = FALSE), 1)
  strt.date <- str_sub(nc.first, start = 1, end = 8)
  end.date <- str_sub(nc.last, start = 1, end = 8)

  ncFun <- function(nc.file = nc.file, csv.dir = csv.dir) {
    nc <- nc_open(nc.file)
    path.len <- nchar(nc.dir) + 1
    name.stem <- substr(nc.file, path.len + 16, path.len + 72)
    date.stamp <- substr(nc.file, path.len + 1, path.len + 8)
    sst <- ncvar_get(nc, varid = "analysed_sst") %>%
      round(4)
    dimnames(sst) <- list(lon = nc$dim$lon$vals,
                          lat = nc$dim$lat$vals)
    nc_close(nc)
    sst <-
      as.data.frame(melt(sst, value.name = "temp"), row.names = NULL) %>%
      mutate(t = ymd(date.stamp)) %>%
      na.omit()
    fwrite(sst,
           file = paste0(csv.dir, "/", name.stem, "-", strt.date, "-", end.date, ".csv"),
           append = TRUE, col.names = FALSE)
    rm(sst)
  }

  llply(nc.list, ncFun, csv.dir = csv.dir, .parallel = FALSE)
}


# RUN THE FUNCTION --------------------------------------------------------

# If everything works according to plan, all that's required is to execute
# this line as is after specifying `nc.dir` and `csv.dir` paths, above;
# ONE file should appear in the `csv.dir`
system.time(ncRead(nc.dir, csv.dir))
