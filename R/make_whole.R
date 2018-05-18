#' Constructs a continuous, uninterrupted time series of temperatures.
#'
#' Takes a series of dates and temperatures, and if irregular (but ordered), inserts
#' missing dates and fills correpsonding temperatures with NAs.
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
#'
#' @details
#' \enumerate{
#' \item Upon import, the package uses `zoo` and `lubridate` to process the input
#' date and temperature data. It reads in daily data with the time vector
#' specified as either \code{POSIXct} or \code{Date}  (e.g. "1982-01-01 02:00:00" or
#' "1982-01-01"). The data may be an irregular time series, but date must be
#' ordered. The function constructs a complete time series from the start date
#' to the end date, and fills in the regions in the time series where temperature
#' data are missing, with NAs in the temperature vector. There must only be one
#' temperature value per day otherwise the function will fail. It is up to the
#' user to calculate daily data from sub-daily measurements. Leap years are
#' automatically accommodated by 'zoo'.
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
#' \item Previous versions of \code{make_whole} tested to see if some rows are
#' duplicated, or if replicate temperature readings are present per day, but this
#' has now been disabled. Should the user be concerned about such repeated
#' measurements, we suggest that the necessary checks and fixes are implemented
#' prior to feeding the time series to \code{make_whole} (this is usually done
#' via \code{\link{ts2clm}}).
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
#' @export
#'
#' @author Smit, A. J.
#'
#' @examples
#' library(dplyr); library(tidyr); library(lubridate)
#' ts_dat <- make_whole(sst_WA) # default columns "t" and "temp", in that order
#' clim_start <- "1983-01-01"
#' clim_end <- "2012-12-31"
#' ts_dat %>%
#' filter(t >= clim_start & t <= clim_end) %>%
#'   mutate(t = year(t)) %>%
#'   spread(t, temp) %>%
#'   filter(doy >= 55 & doy <= 65)
make_whole <- function(data, x = t, y = temp) {

  temp <- NULL

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)

  t_series <- zoo::zoo(ts_y, ts_x)
  ts_x_ser <- seq(stats::start(t_series), stats::end(t_series), by = "1 day")
  ts_x_ser <- zoo::zoo(rep(NA, length(ts_x_ser)), order.by = ts_x_ser)
  t_series <- merge(ts_x_ser, t_series)[, 2]

  feb28 <- 59
  v_date <- as.Date(stats::time(t_series))
  v_doy <- lubridate::yday(t_series)
  v_doy <- as.integer(ifelse(
    lubridate::leap_year(lubridate::year(t_series)) == FALSE,
    ifelse(v_doy > feb28, v_doy + 1, v_doy),
    v_doy)
  )
  v_ts_y <- as.numeric(t_series)

  t_series <- tibble::tibble(doy = v_doy,
                     date = v_date,
                     ts_y = v_ts_y)

  names(t_series)[2] <- paste(substitute(x))
  names(t_series)[3] <- paste(substitute(y))

  return(t_series)
}
