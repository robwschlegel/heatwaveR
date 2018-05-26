library(tidyverse)
library(profvis)
library(heatwaveR)
library(microbenchmark)
library(data.table)
library(imputeTS)

data <- sst_WA
ts_x <- data$t
ts_y <- data$temp
ts_xy <- data.table(ts_x, ts_y)

climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 1
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
clim_start <- "1983-01-01"
clim_end <- "2012-12-31"

ts_whole <- make_whole_fast(ts_xy, x = ts_x, y = ts_y)

# ts_wide <- clim_spread(ts_whole, clim_start, clim_end, windowHalfWidth)

.NA2mean <- function(x) {
  z <- round(mean(x, na.rm = TRUE), 2)
  x[is.na(x)] <- z
  return(x)
}

data <- ts_whole

ts_whole <- data[ts_x %between% c(clim_start, clim_end)]
data.table::setDT(ts_whole)[, ts_x := format(as.Date(ts_x), "%Y") ]
ts_spread <- data.table::dcast(ts_whole, doy ~ ts_x, value.var = "ts_y")
ts_spread_sub <- data.table::data.table((sapply(ts_spread[59:61, ], function(x) .NA2mean(x))))
ts_spread[60, ] <- ts_spread_sub[2, ]

begin_pad <- utils::tail(ts_spread, windowHalfWidth)
end_pad <- utils::head(ts_spread, windowHalfWidth)
l <- list(begin_pad, ts_spread, end_pad)
ts_spread <- data.table::rbindlist(l)
rm(begin_pad); rm(end_pad); rm(l)




# ind <- which(ts_whole$ts_y %in% sample(ts_whole$ts_y, 100))[,2]
# ts_whole$ts_y[ind] <- NA

# benchmarks --------------------------------------------------------------



profvis(res_climF <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
out <- detect_event(res_climF)


microbenchmark(ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = TRUE),
               ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), robust = FALSE))
