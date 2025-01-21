library(tidyverse)
library(heatwaveR)

ts_dat <- make_whole_fast(sst_WA)
colnames(ts_dat) <- c("doy", "ts.x", "ts.y")

data <- ts_dat
clim_start <- climatology_start <- "1983-01-01"
clim_end <- climatology_end <- "2012-12-31"
pctile <- 90
window_half_width <- 5
smooth_percentile <- TRUE
smooth_percentile_width <- 31
clim_only <- FALSE
min_duration <- 5
join_across_gaps <- TRUE
max_gap <- 2
max_pad_length <- 3
cold_spells <- FALSE
diff_baseline <- FALSE
baseline_data <- NULL

t_series <- as_tibble(ts_dat)
ts.xy <- t_series
colnames(ts.xy) <-  c("doy", "ts.x", "ts.y")

tDat <- ts.xy %>%
  dplyr::filter(ts.x >= clim_start & ts.x <= clim_end) %>%
  dplyr::mutate(ts.x = lubridate::year(ts.x)) %>%
  tidyr::spread(ts.x, ts.y)

tDat[59:61, ] <- zoo::na.approx(tDat[59:61, ], maxgap = 1, na.rm = TRUE)
tDat <- rbind(utils::tail(tDat, window_half_width),
              tDat, utils::head(tDat, window_half_width))

seas_clim_year <- rep(NA, nrow(tDat))
thresh_clim_year <- rep(NA, nrow(tDat))
var_clim_year <- rep(NA, nrow(tDat))

for (i in (window_half_width + 1):((nrow(tDat) - window_half_width))) {
  seas_clim_year[i] <-
    mean(
      c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
      na.rm = TRUE)
  thresh_clim_year[i] <-
    stats::quantile(
      c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
      probs = pctile/100,
      type = 7,
      na.rm = TRUE,
      names = FALSE
    )
  var_clim_year[i] <-
    stats::sd(
      c(t(tDat[(i - (window_half_width)):(i + window_half_width), 2:ncol(tDat)])),
      na.rm = TRUE
    )
}

len_clim_year <- 366
clim <-
  data.frame(
    doy = tDat[(window_half_width + 1):((window_half_width) + len_clim_year), 1],
    seas_clim_year = seas_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
    thresh_clim_year = thresh_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
    var_clim_year = var_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)]
  )

t_series <- merge(data, clim, by = "doy") # <--- construct a 366-day climatology and merge with doy in data

# Load and prep data
header <- read.table('~/Downloads/CIM-ENCIMAT_CTD.tab', skip = 16, nrows = 1, header = FALSE, sep ='\t', stringsAsFactors = FALSE)
pier_df <- read.table('~/Downloads/CIM-ENCIMAT_CTD.tab', skip = 17, header = FALSE, sep ='\t')
colnames(pier_df) <- unlist(header)

# Hourly means
pier_df_hourly <- pier_df |>
  mutate(t = str_replace(`Date/Time`, "T", " "), # Remove 'T' from date string
         t = as.POSIXct(t, format = "%Y-%m-%d %H", tz = "UTC")) |>  # Convert to POSIXct and keep only hours
  dplyr::rename(temp = `Temp [°C]`) |>  # Rename 'Temp [°C]' to 't'
  dplyr::select(t, temp) |>
  summarise(temp = mean(temp, na.rm = TRUE), .by = "t") # Calculate mean temperature per hour

# Daily means
pier_df_daily <- pier_df |>
  mutate(t = str_replace(`Date/Time`, "T", " "), # Remove 'T' from date string
         t = as.Date(t, tz = "UTC")) |>  # Convert to POSIXct and keep only hours
  dplyr::rename(temp = `Temp [°C]`) |>  # Rename 'Temp [°C]' to 't'
  dplyr::select(t, temp) |>
  summarise(temp = mean(temp, na.rm = TRUE), .by = "t") # Calculate mean temperature per day

# Hourly results
ts_clima_hourly <- ts2clm(pier_df_hourly, climatologyPeriod = c(as.POSIXct("2014-01-01 00"), as.POSIXct("2018-12-31 23")))
mhw_hourly <- detect_event(ts_clima_hourly, minDuration = 120, maxGap = 48) # 5 day duration X 24 hours, 2 day gap X 24 hours
mhw_hourly_event <- mhw_hourly$event

# Daily results
ts_clima_daily <- ts2clm(pier_df_daily, climatologyPeriod = c(as.Date("2014-01-01"), as.Date("2018-12-31")))
mhw_daily <- detect_event(ts_clima_daily, minDuration = 5) # 5 days
mhw_daily_event <- mhw_daily$event

