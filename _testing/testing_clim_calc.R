library(data.table)
library(tidyverse)
library(profvis)
library(heatwaveR)
library(microbenchmark)
library(imputeTS)

climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 1
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
minDuration <- 5
maxGap <- 2
joinAcrossGaps <- TRUE
criterion_column <- "threshCriterion"
minDuration <- 3
gaps <- FALSE

# sst <- fread("/Users/ajsmit/Dropbox/R/Benguela_MUR/sst_test.csv")
data <- sst_WA
# data <- sst
# data <- sst_WA %>%
#   dplyr::mutate(month = month(t)) %>%
#   dplyr::filter(month != 1) %>%
#   dplyr::select(-month)
ts_x <- data$t
ts_y <- data$temp
ts_xy <- data.table::data.table(ts_x, ts_y)

source("R/make_whole_fast.R")
source("R/make_whole.R")
ts_whole <- make_whole(ts_xy, x = ts_x, y = ts_y)

source("R/na_interp.R")
ts_interped <- na_interp(doy = ts_whole$doy,
                         x = ts_whole$ts_x,
                         y = ts_whole$ts_y,
                         maxPadLength = maxPadLength)

### begin test
# ts_interped %>%
#   dplyr::filter(ts_x >= "2006-01-01" & ts_x <= "2006-01-10")
### end test

clim_start <- climatologyPeriod[1]
clim_end <- climatologyPeriod[2]

source("R/clim_spread.R")

ts_wide <- clim_spread(ts_interped, clim_start, clim_end, windowHalfWidth)
save(ts_wide, file = "/Users/ajsmit/Dropbox/R/Rcpp_test/_test/ts_wide.RData")
ts_wide_NA <- ts_wide
ts_wide_NA[15,2] <- NA # <-- introduces NAs
ts_wide_NA[15,] <- NA # <-- introduces NAs
save(ts_wide_NA, file = "/Users/ajsmit/Dropbox/R/Rcpp_test/_test/ts_wide_NA.RData")

# source("src/clim_calc.cpp") # does not work with NAs
source("R/clim_calc.R") # works
ts_mat_cpp <- heatwaveR:::clim_calc_cpp(ts_wide, windowHalfWidth, pctile)
ts_mat_r <- clim_calc(ts_wide, windowHalfWidth, pctile)

# benchmarks --------------------------------------------------------------

profvis(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)))
microbenchmark(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE)))

microbenchmark(
proto_event(t_series, criterion_column = 5,
                       minDuration = minDuration, maxGap = maxGap)
)


ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE)
