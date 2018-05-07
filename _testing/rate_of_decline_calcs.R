# Below is the original R code that was committed on 21 May 2016. I have updated it according to the edits Eric made to the python code on 5 May 2016.
# Eric edited lines 401 and 406 of his python code marineHeatWaves.py, and I have updated my original R code accordingly in lines 27 and 29, below.
# I have not tested this update. Subsequently you have changed the detect() function somewhat, and the new version (yours) is below my original
# code. Your edits must reflect the changes I have made to lines 27 and 29.

# rate of decline
D <- mhw_rel_seas[events$index_stop]
E <- t_series$temp[events$index_stop + 1]
F <- t_series$seas_clim_year[events$index_stop + 1]
mhw_rel_seas_end <- 0.5 * (D + E - F)
stopType <- ifelse(
  events$index_stop < nrow(t_series),
  "case4",
  # event does not finish at end of time series
  ifelse(
    events$index_stop == nrow(t_series) &
      difftime(events$date_peak, t_series[nrow(t_series), "date"], units = "days") < 0,
    "case5",
    # event finishes at end of time series
    "case6" # peak also at begn of time series, assume onset time = 1 day
  )
)[nrow(events)]

rateDecline <- function(x, type) {
  switch(
    type,
    case4 = (x$int_max - mhw_rel_seas_end) / (as.numeric(difftime(difftime(x$date_stop, x$date_start, units = "days"), x$date_peak, units = "days")) + 0.5),
    case5 = (x$int_max - mhw_rel_seas_end) / 1,
    case6 = (x$int_max - mhw_rel_seas_end) / as.numeric(difftime(difftime(x$date_stop, x$date_start, units = "days"), x$date_peak, units = "days"))
  )
}
events$rate_decline <- rateDecline(events, stopType)



# Rob's edits that resulted in a much shorter rate of decline calculation... I think this was done to correct problems that resulted from NAs creeping
# in under certain circumstances when the peaks occurred right at the end (?) of time series, which is what Eric also corrected his code for.

D <- mhw_rel_seas[events$index_stop]
E <- t_series$ts_y[events$index_stop + 1]
F <- t_series$seas_clim_year[events$index_stop + 1]
mhw_rel_seas_end <- 0.5 * (D + E - F)

events$rate_decline <- ifelse(
  events$index_stop < nrow(t_series),
  (events$int_max - mhw_rel_seas_end) / (as.numeric(difftime(events$date_stop, events$date_peak, units = "days")) + 0.5),
  NA
)
