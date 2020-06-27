# This script downloads and preps the three OISST time series used in the package


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(rerddap)
library(doParallel); registerDoParallel(cores = 7)
# library(ncdf4)

# Most up-to-date info
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Date download range by start and end dates per year
dl_years <- data.frame(date_index = 1:5,
                       start = as.Date(c("1982-01-01", "1990-01-01",
                                         "1998-01-01", "2006-01-01", "2014-01-01")),
                       end = as.Date(c("1989-12-31", "1997-12-31",
                                       "2005-12-31", "2013-12-31", "2019-12-31")))

# Functions ---------------------------------------------------------------

# Download the data
OISST_pixel_dl <- function(time_df, lon, lat){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180",
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                       time = c(time_df$start, time_df$end),
                       zlev = c(0, 0),
                       latitude = rep(lat, 2),
                       longitude = rep(lon, 2),
                       fields = "sst")$data %>%
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time, temp = sst) %>%
    select(t, temp) %>%
    na.omit()
}

# Wrapper to download data in ~5 year chunks as this is the rough ERDDAP limit
OISST_time_wrap <- function(lon, lat){
  OISST_sub <- plyr::ddply(dl_years, "date_index", OISST_pixel_dl, .parallel = T,
                           lon = lon,lat = lat) %>%
  dplyr::select(-date_index)
  # OISST_sub <- dl_years %>%
  #   group_by(date_index) %>%
  #   group_modify(~OISST_sub_dl(.x, lon = lon, lat = lat)) %>%
  #   ungroup() %>%
  #   select(t, temp)
}


# Download ----------------------------------------------------------------

# Thee take roughly 100 minutes each
sst_WA_new <- OISST_time_wrap(112.5, -29.5)
sst_Med_new <- OISST_time_wrap(9, 43.5)
sst_NW_Atl_new <- OISST_time_wrap(-67, 43)


# Check that the correct data were downloaded -----------------------------

data(sst_WA)
data(sst_Med)
data(sst_NW_Atl)

# Join to test
sst_WA_test <- left_join(sst_WA, sst_WA_new, by = c("t")) %>%
  mutate(test_col = temp.x - temp.y)
sst_Med_test <- left_join(sst_Med, sst_Med_new, by = c("t")) %>%
  mutate(test_col = temp.x - temp.y)
sst_NW_Atl_test <- left_join(sst_NW_Atl, sst_NW_Atl_new, by = c("t")) %>%
  mutate(test_col = temp.x - temp.y)

# If everything checks out prepare to overwrite the old files
sst_WA <- sst_WA_new
sst_Med <- sst_Med_new
sst_NW_Atl <- sst_NW_Atl_new


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


# The Algiers data from Mahmoud -------------------------------------------

# Load the raw file received via e-mail
load("data-raw/Algiers.RData")

# Combine the year, month, day columns into a date column
Algiers <- Algiers %>%
  unite(year, month, day, sep = "-", col = "t") %>%
  mutate(t = as.Date(t)) %>%
  rename(tMax = TX, tMin = TN)

# Add to package
usethis::use_data(Algiers, overwrite = T)

