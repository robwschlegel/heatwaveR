library(tidyverse)
library(profvis)
library(heatwaveR)

res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"), smoothPercentile = FALSE)
out <- detect_event(res_clim)

data <- sst_WA
ts_x <- data$t
ts_y <- data$temp
climatologyPeriod <- c("1983-01-01", "2012-12-31")
maxPadLength <- 3
windowHalfWidth <- 5
pctile <- 90
smoothPercentile <- TRUE
smoothPercentileWidth <- 31
clmOnly <- TRUE
clim_start <- "1983-01-01"
clim_end <- "2012-12-31"

ts_xy <- tibble::tibble(ts_x, ts_y)
# rm(ts_x); rm(ts_y)

ts_whole <- make_whole(ts_xy, x = ts_x, y = ts_y)

# matrices are faster; create one
ts_wide <- clim_spread(ts_whole, clim_start, clim_end, windowHalfWidth)

# test --------------------------------------------------------------------

junk <- tibble(c_ind = seq(0, nrow(ts_wide)-1, 1),
               ind = c(rep("pad", windowHalfWidth), c(1:366), rep("pad", windowHalfWidth)),
               temp = ts_wide[,1])

# test end ----------------------------------------------------------------

# this is meant to make the climatology for doy 1, using a

mean(c(ts_wide[1:11, ]))

# compare with

res_clim[1, ]

# C++
c_out <- fun3(ts_wide, 5)
head(c_out)



