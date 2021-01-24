#' Constructs a continuous, uninterrupted time series of temperatures.
#'
#' Takes a series of dates and temperatures, and if irregular (but ordered), inserts
#' missing dates and fills corresponding temperatures with NAs.
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
#' specified as either \code{POSIXct} or \code{Date} (e.g. "1982-01-01 02:00:00" or
#' "1982-01-01"). The data may be an irregular time series, but date must be
#' ordered. The function constructs a complete time series from the start date
#' to the end date, and fills in the regions in the time series where temperature
#' data are missing with NAs in the temperature vector. There must only be one
#' temperature value per day otherwise the function takes the mean of multiple
#' values. It is up to the user to calculate daily data from sub-daily
#' measurements. Leap years are automatically accommodated by this function.
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
#' \item It is recommended that a climatology period of at least 30 years is
#' specified in order to capture any decadal thermal periodicities.
#'
#' \item A faster version of this function called \code{make_whole_fast}
#' is enabled by default in \code{ts2clm}, and we assume (hopefully correctly)
#' that the user is certain that her data do not have missing rows (dates) or
#' duplicated values. This slower but more robust function (i.e. \code{make_whole})
#' may be selected as an argument to \code{ts2clm} in case the data may have some
#' issues.
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
make_whole <- function(data, x = t, y = temp) {

  temp <- doy <- NULL
  feb28 <- 59

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)
  dat <- data.frame(ts_x, ts_y)
  rm(list = c("ts_x", "ts_y"))

  dat <- dat %>%
    dplyr::group_by(ts_x) %>%
    dplyr::summarise(ts_y = mean(ts_y, na.rm = TRUE)) %>%
    dplyr::ungroup()

  ser <- data.frame(ts_x = seq(min(lubridate::ymd(dat$ts_x)),
                               max(lubridate::ymd(dat$ts_x)), by = "1 day"))

  t_series <- ser %>%
    dplyr::left_join(y = dat, by = "ts_x") %>%
    dplyr::mutate(doy = lubridate::yday(ts_x),
           date = as.Date(as.POSIXct(ts_x)),
           ts_y = as.numeric(ts_y)) %>%
    dplyr::select(doy, date, ts_y, -ts_x)
  rm(list = c("ser", "dat"))

  # Lines 82-91 may be replaced by this (below) but this requires tidyr :-(
  # However, tidyr is listed as 'Suggests', which means it is sometimes loaded by other packages
  # as and when needed. This function (`make_whole`) is not used by default (it needs to be enabled
  # when needed if `make_whole_fast` fails, so perhaps this is good enough reason to use the more
  # efficient code that relies on tidyr instead of lines 82-91...?)
  #
  # t_series <- dat %>%
  #   dplyr::rename(date = ts_x) %>%
  #   dplyr::mutate(date = as.Date(as.POSIXct(date)),
  #                 ts_y = as.numeric(ts_y)) %>%
  #   tidyr::complete(date = seq.Date(min(lubridate::ymd(ts_x)), max(lubridate::ymd(ts_x)), by = "day")) %>%
  #   dplyr::mutate(doy = lubridate::yday(date)) %>%
  #   dplyr::select(doy, date, ts_y)

  t_series$doy <- as.integer(ifelse(
    lubridate::leap_year(lubridate::year(t_series$date)) == FALSE,
    ifelse(t_series$doy > feb28, t_series$doy + 1, t_series$doy),
    t_series$doy)
  )

  names(t_series)[2] <- paste(substitute(x))
  names(t_series)[3] <- paste(substitute(y))

  return(t_series)
}
