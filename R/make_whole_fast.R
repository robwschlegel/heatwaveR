#' Constructs a continuous, uninterrupted time series of temperatures (faster).
#'
#' Takes a series of dates and temperatures, and if irregular (but ordered), inserts
#' missing dates and fills correpsonding temperatures with NAs.
#'
#' @importFrom data.table := data.table
#' @importFrom dplyr %>%
#'
#' @param data A data frame with columns for date (\code{ts_x}) and
#' temperature (\code{ts_y}) data. Ordered daily data are expected, and
#' although missing values (NA) can be accommodated, the function is only
#' recommended when NAs occur infrequently, preferably at no more than three
#' consecutive days.
#'
#' @details
#' \enumerate{
#' \item This function reads in daily data with the time vector specified as
#'  \code{Date} (e.g. "1982-01-01").
#'
#' \item It is up to the user to calculate daily data from sub-daily measurements.
#' Leap years are automatically accommodated by this function.
#'
#' \item This function can handle some missing days, but this is not a
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
#' }
#'
#' @return The function will return a data frame with three columns. The column
#' headed \code{doy} (day-of-year) is the Julian day running from 1 to 366, but
#' modified so that the day-of-year series for non-leap-years runs 1...59 and
#' then 61...366. For leap years the 60th day is February 29. The \code{ts_x}
#' column is a series of dates of class \code{Date}, while \code{y} is the
#' measured variable. This time series will be uninterrupted and continuous daily
#' values between the first and last dates of the input data.
#'
#' @author Smit, A. J., Schlegel, R. W.
#'
make_whole_fast <- function(data) {

  feb28 <- 59

  # create full, complete time series for joining against
  date_start <- lubridate::ymd(utils::head(data$ts_x, 1))
  date_end <- lubridate::ymd(utils::tail(data$ts_x, 1))
  ts_full <- data.table::data.table(ts_x = seq.Date(date_start, date_end, "day"))

  ts_merged <- merge(ts_full, data, all.x = TRUE)
  rm(ts_full)

  v_date <- ts_merged$ts_x
  v_doy <- lubridate::yday(v_date)
  v_doy <- as.integer(ifelse(
    lubridate::leap_year(lubridate::year(ts_merged$ts_x)) == FALSE,
    ifelse(v_doy > feb28, v_doy + 1, v_doy),
    v_doy)
  )
  v_ts_y <- as.numeric(ts_merged$ts_y)

  t_series <- data.table::data.table(doy = v_doy,
                                     ts_x = v_date,
                                     ts_y = v_ts_y)
  rm(list = c("v_date", "v_doy", "v_ts_y"))

  return(t_series)
}
