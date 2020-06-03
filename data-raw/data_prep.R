# This script downloads and preps the three OISST time series used in the package


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(heatwaveR)
library(tidync)


# File info ---------------------------------------------------------------

# First we tell R where the data are on the interwebs
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# Then we pull that into a happy format
OISST_url_month_get <- getURL(OISST_url_month)

# Now we strip away all of the unneeded stuff to get just the months of data that are available
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>%
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>%
  mutate(months = gsub("-", "", substr(months, 1, 7))) %>%
  na.omit()

# Find the URLs for each individual day of data
OISST_url_daily <- function(target_month){
  OISST_url <- paste0(OISST_url_month, target_month,"/")
  OISST_url_get <- getURL(OISST_url)
  OISST_table <- data.frame(files = readHTMLTable(OISST_url_get, skip.rows = 1:2)[[1]]$Name) %>%
    mutate(files = as.character(files)) %>%
    filter(grepl("avhrr", files)) %>%
    mutate(t = lubridate::as_date(sapply(strsplit(files, "[.]"), "[[", 2)),
           full_name = paste0(OISST_url, files))
  return(OISST_table)
}

# Here we collect the URLs for every day of data available from 2019 onwards
OISST_filenames <- plyr::ldply(OISST_months$months, .fun = OISST_url_daily)



# Download full OISST dataset ---------------------------------------------

# This function will go about downloading each day of data as a NetCDF file
OISST_url_daily_dl <- function(target_URL){
  dir.create("~/data/OISST", showWarnings = F)
  file_name <- paste0("~/data/OISST/",sapply(strsplit(target_URL, split = "/"), "[[", 10))
  if(!file.exists(file_name)) download.file(url = target_URL, method = "libcurl", destfile = file_name)
}

# Set cores
doParallel::registerDoParallel(cores = 7)

# Catch'em all
plyr::l_ply(OISST_filenames$full_name, .fun = OISST_url_daily_dl, .parallel = T)


# Load single pixel -------------------------------------------------------

# Function for loading a single pixel
OISST_pixel <- function(file_name, lon1, lat1){
  OISST_dat <- tidync(file_name) %>%
    hyper_filter(lon = lon == lon1,
                 lat = lat == lat1) %>%
    hyper_tibble() %>%
    select(lon, lat, time, sst) %>%
    dplyr::rename(t = time, temp = sst) %>%
    mutate(t = as.Date(t, origin = "1978-01-01"))
  return(OISST_dat)
}

# Convert the NW Atl lon to the correct format
# -67+360

sst_WA <- OISST_lon_rep(112.5, -29.5)
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

