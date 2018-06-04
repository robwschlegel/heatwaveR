#' Constructs a continuous, uninterrupted time series of temperatures (faster).
#'
#' Takes a series of dates and temperatures, and if irregular (but ordered), inserts
#' missing dates and fills correpsonding temperatures with NAs.
#'
#' @importFrom data.table := data.table
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
#'
#' @details
#' \enumerate{
#' \item This function reads in daily data with the time vector specified as
#' either \code{POSIXct} or \code{Date} (e.g. "1982-01-01 02:00:00" or
#' "1982-01-01").
#'
#' \item It is up to the user to calculate daily data from sub-daily measurements.
#' Leap years are automatically accommodated by this function.
#'
#' \item This function can handle some of missing days, but this is not a
#' licence to actually use these data for the detection of anomalous thermal
#' events. Hobday et al. (2016) recommend gaps of no more than 3 days, which
#' may be adjusted by setting the \code{maxPadLength} argument of the
#' \code{\link{ts2clm}} function. The longer and more frequent the gaps become
#' the lower the fidelity of the annual climatology and threshold that can be
#' calculated, which will not only have repercussions for the accuracy at which
#' the event metrics can be determined, but also for the number of events that
#' can be detected.
#'
#' \item The original \code{make_whole} tests to see if some rows are
#' duplicated, or if replicate temperature measurements are present per day. In
#' \code{make_whole_fast} (this function) this has been disabled; also,
#' the latter function lacks the facility to check if the time series is complete
#' and regular (i.e. no missing values in the date vector). Effectively,
#' we now only set up the day-of-year (doy) vector in \code{make_whole_fast}.
#' Should the user be concerned about the potential for repeated measurements
#' or worry that the time series is irregular, we suggest that the necessary
#' checks and fixes are implemented prior to feeding the time series to \code{ts2clim}
#' via \code{make_whole_fast}, or to use \code{make_whole} instead. For very large
#' gridded temperature records it probably makes a measurable difference if the
#' 'fast' version is used, but it might prevent \code{\link{detect_event}}
#' from failing should some gridded cells contain missing rows or some duplicated
#' values. When using the fast algorithm, we assume that the user has done all
#' the necessary work to ensure that the time vector is regular and without
#' repeated measurements beforehand.
#'
#' \item It is recommended that a climatology period of at least 30 years is specified
#' in order to capture any decadal thermal periodicities.
#' }
#'
#' @return The function will return a data frame with three columns. The column
#' headed \code{doy} (day-of-year) is the Julian day running from 1 to 366, but
#' modified so that the day-of-year series for non-leap-years runs 1...59 and
#' then 61...366. For leap years the 60th day is February 29. See the example,
#' below. The other two columns take the names of \code{x} and \code{y}, if supplied,
#' or it will be \code{t} and \code{temp} in case the default values were used.
#' The \code{x} (or \code{t}) column is a series of dates of class \code{Date},
#' while \code{y} (or \code{temp}) is the measured variable. This time series will
#' be uninterrupted and continuous daily values between the first and last dates
#' of the input data.
#'
#' @author Smit, A. J.
#'
make_whole_fast <- function(data, x = t, y = temp) {

  temp <- NULL
  feb28 <- 59

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)

  v_date <- as.Date(ts_x)
  v_doy <- lubridate::yday(v_date)
  v_doy <- as.integer(ifelse(
    lubridate::leap_year(lubridate::year(ts_x)) == FALSE,
    ifelse(v_doy > feb28, v_doy + 1, v_doy),
    v_doy)
  )
  v_ts_y <- as.numeric(ts_y)

  t_series <- data.table::data.table(doy = v_doy,
                                     date = v_date,
                                     ts_y = v_ts_y)

  names(t_series)[2] <- paste(substitute(x))
  names(t_series)[3] <- paste(substitute(y))

  return(t_series)
}
