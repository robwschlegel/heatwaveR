#' Detect the climatology for a time series
#'
#' Applies a climatology detecting algorithm to a time series with
#' several built-in options. The output of this function may then be
#' used directly with the \code{\link{detect_event}} function.
#'
#' @importFrom dplyr %>%
#'
#' @param data A data frame with columns for date and temperature data.
#' Ordered daily data are expected, and although missing values (NA) can be
#' accommodated, the function is only recommended when NAs occur infrequently,
#' preferably at no more than 3 consecutive days.
#' @param x A column with the daily time vector (see details). For backwards
#' compatibility, the column is named \code{t} by default.
#' @param y A column with the response vector. RmarineHeatWaves version <= 0.15.9
#' assumed that this would be daily seawater temperatures, but as of version 0.16.0
#' it may be any arbitrary measurement taken at a daily frequency. The default
#' remains temperature, and the default column name is therefore \code{temp}, again
#' hopefully ensuring backwards compatibility.
#' @param climatology_start Required. The start date for the period across which
#' the (varying by day-of-year) seasonal cycle and extremes threshold are
#' calculated.
#' @param climatology_end Required. The end date for the period across which
#' the (varying by day-of-year) seasonal cycle and extremes threshold are
#' calculated.
#' @param pctile Threshold percentile (\%) for detection of extreme values.
#' Default is \code{90}th percentile. Please see \code{cold_spells} for more
#' information about the calculation of marine cold-spells.
#' @param window_half_width Width of sliding window about day-of-year (to one
#' side of the center day-of-year) used for the pooling of values and
#' calculation of climatology and threshold percentile. Default is \code{5}
#' days, which gives a window width of 11 days centered on the 6th day of the
#' series of 11 days.
#' @param smooth_percentile Boolean switch selecting whether to smooth the
#' climatology and threshold percentile timeseries with a moving average of
#' \code{smooth_percentile_width}. Default is \code{TRUE}.
#' @param smooth_percentile_width Full width of moving average window for smoothing
#' climatology and threshold. Default is \code{31} days.
#' @param max_pad_length Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{max_pad_length} will be left as \code{NA}. Set as an
#' integer. The default is \code{3} days.
#'
#' @export
detect_clim <- function(data,
                        x = t,
                        y = temp,
                        climatology_start,
                        climatology_end,
                        pctile = 90,
                        window_half_width = 5,
                        smooth_percentile = TRUE,
                        smooth_percentile_width = 31,
                        max_pad_length = 3) {

  if (missing(climatology_start))
    stop("Oops! Please provide BOTH start and end dates for the climatology.")
  if (missing(climatology_end))
    stop("Bummer! Please provide BOTH start and end dates for the climatology.")
  if(!(is.numeric(pctile)))
    stop("Please ensure that 'pctile' is a numeric/integer value.")
  if(!(is.numeric(window_half_width)))
    stop("Please ensure that 'window_half_width' is a numeric/integer value.")
  if(!(is.logical(smooth_percentile)))
    stop("Please ensure that 'smooth_percentile' is either TRUE or FALSE.")
  if(!(is.numeric(smooth_percentile_width)))
    stop("Please ensure that 'smooth_percentile_window' is a numeric/integer value.")
  if(!(is.numeric(max_pad_length)))
    stop("Please ensure that 'max_pad_length' is a numeric/integer value.")

  temp <- NULL

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)
  ts_xy <- tibble::tibble(ts_x, ts_y)
  rm(ts_x); rm(ts_y)

  ts_whole <- make_whole(ts_xy, x = ts_x, y = ts_y)

  clim_start <- climatology_start
  if (ts_whole$ts_x[1] > clim_start)
    stop(paste("The specified start date precedes the first day of series, which is ",
               ts_whole$ts_x[1]))

  clim_end <- climatology_end
  if (clim_end > ts_whole$ts_x[nrow(ts_whole)])
    stop(paste("The specified end date follows the last day of series, which is ",
               ts_whole$ts_x[nrow(whole)]))

  ts_wide <- heatwaveR:::clim_spread(ts_whole, clim_start, clim_end, window_half_width)

  ts_clim <- heatwaveR:::clim_calc(ts_wide, window_half_width)

  ts_smooth <- heatwaveR:::smooth_percentile(ts_clim, smooth_percentile_width)

  ts_res <- ts_whole %>%
    dplyr::inner_join(ts_smooth, by = "doy")

  # names(ts_res)[1] <- paste(substitute(doy))
  names(ts_res)[2] <- paste(substitute(x))
  names(ts_res)[3] <- paste(substitute(y))
  return(ts_res)
}
