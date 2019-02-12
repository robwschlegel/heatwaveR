# This script downloads and preps the three OISST time series used in the package


# Libraries ---------------------------------------------------------------

library(dplyr)
library(heatwaveR)
library(rerddap)
library(ncdf4)

# Most up-to-date info
rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon",
              url = "https://www.ncei.noaa.gov/erddap/")


# Functions ---------------------------------------------------------------

# Download the data
OISST_lon_dl <- function(times, lon, lat){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon",
                       url = "https://www.ncei.noaa.gov/erddap/",
                       time = times,
                       depth = c(0, 0),
                       latitude = rep(lat, 2),
                       longitude = rep(lon, 2),
                       fields = "sst")
}

# Prep a single lon download
OISST_lon_prep <- function(times, lon, lat){

  # Open the NetCDF connection
  nc_file <- OISST_lon_dl(times, lon, lat)

  nc <- nc_open(nc_file$summary$filename)

  # Extract the SST values and add the lon/lat/time dimension names
  res <- ncvar_get(nc, varid = "sst")
  dimnames(res) <- list(t = nc$dim$time$vals)

  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>%
    mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
           temp = round(temp, 2))

  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

# Wrapper to download data in ~5 year chunks as this is the erddap limit
OISST_lon_rep <- function(lon, lat){
  # download
  OISST_1 <- OISST_lon_prep(c("1982-01-01T00:00:00Z", "1986-12-31T00:00:00Z"), lon, lat)
  OISST_2 <- OISST_lon_prep(c("1987-01-01T00:00:00Z", "1991-12-31T00:00:00Z"), lon, lat)
  OISST_3 <- OISST_lon_prep(c("1992-01-01T00:00:00Z", "1996-12-31T00:00:00Z"), lon, lat)
  OISST_4 <- OISST_lon_prep(c("1997-01-01T00:00:00Z", "2001-12-31T00:00:00Z"), lon, lat)
  OISST_5 <- OISST_lon_prep(c("2002-01-01T00:00:00Z", "2006-12-31T00:00:00Z"), lon, lat)
  OISST_6 <- OISST_lon_prep(c("2007-01-01T00:00:00Z", "2011-12-31T00:00:00Z"), lon, lat)
  OISST_7 <- OISST_lon_prep(c("2012-01-01T00:00:00Z", "2016-12-31T00:00:00Z"), lon, lat)
  OISST_8 <- OISST_lon_prep(c("2017-01-01T00:00:00Z", "2018-12-31T00:00:00Z"), lon, lat)

  # Finish
  OISST_all <- rbind(OISST_1, OISST_2, OISST_3, OISST_4,
                     OISST_5, OISST_6, OISST_7, OISST_8)
  return(OISST_all)
}


# Download ----------------------------------------------------------------

# Convert the NW Atl lon to the correct format
# -67+360

sst_WA <- OISST_lon_rep(112.5, -29.5)
# sst_WA <- unique(sst_WA) #
sst_Med <- OISST_lon_rep(9, 43.5)
sst_NW_Atl <- OISST_lon_rep(293, 43)


# Save --------------------------------------------------------------------

save(sst_WA, file = "data-raw/sst_WA.RData")
save(sst_Med, file = "data-raw/sst_Med.RData")
save(sst_NW_Atl, file = "data-raw/sst_NW_Atl.RData")


# Add to package ----------------------------------------------------------

load("data-raw/sst_Med.RData")
load("data-raw/sst_NW_Atl.RData")
load("data-raw/sst_WA.RData")

usethis::use_data(sst_Med, overwrite = T)
usethis::use_data(sst_NW_Atl, overwrite = T)
usethis::use_data(sst_WA, overwrite = T)

