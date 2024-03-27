#' Constructs a continuous, uninterrupted time series of temperatures (faster).
#'
#' Takes a series of dates and temperatures, and if irregular (but ordered), inserts
#' missing dates and fills corresponding temperatures with NAs.
#'
#' @keywords internal
#'
#' @importFrom data.table := data.table
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
#' The day-of-year (doy) vector is created in \code{make_whole_fast} and
#' inserts rows in cases when the original data set has missing rows for some dates.
#' Should the user be concerned about the potential for repeated measurements
#' or worry that the time series is unordered, we suggest that the necessary
#' checks and fixes are implemented prior to feeding the time series to \code{ts2clim}
#' via \code{make_whole_fast}. When using this fast algorithm,
#' we assume that the user has done all the necessary work to ensure that the time
#' vector is ordered and without repeated measurements beforehand.
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
#' @author Smit, A. J., Villeneuve, A., Schlegel, R. W.
#'
make_whole_fast <- function(data) {

  feb28 <- 59
  feb28_hour <- 1416

  # testing...
  # data <- ts_whole

  year <- doy <- hoy <- ts_x <- NULL

  is_leap_year <- function(year) {
    return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
  }

  date_start <- data$ts_x[1]
  date_end <- data$ts_x[nrow(data)]

  if (inherits(data$ts_x[1], "POSIXct")) {
    ts_full <- data.table::data.table(ts_x = seq.POSIXt(date_start, date_end, "hour"))
    ts_merged <- base::merge(ts_full, data, all.x = TRUE)
    ts_merged[, year := as.integer(format(ts_x, "%Y"))]
    ts_merged[, doy := as.integer(format(ts_x, "%j"))]
    ts_merged[, hoy := (data.table::hour(ts_x)+1) + (data.table::yday(ts_x) - 1) * 24]
    ts_merged[!is_leap_year(year) & doy > feb28, doy := doy + 1]
    ts_merged[!is_leap_year(year) & hoy > feb28_hour, hoy := hoy + 24]
    ts_merged[, year := NULL]
    data.table::setcolorder(ts_merged, c("doy", "hoy", "ts_x", "ts_y"))
  } else {
    ts_full <- data.table::data.table(ts_x = seq.Date(date_start, date_end, "day"))
    ts_merged <- base::merge(ts_full, data, by = "ts_x", all.x = TRUE)
    ts_merged[, year := as.integer(format(ts_x, "%Y"))]
    ts_merged[, doy := as.integer(format(ts_x, "%j"))]
    ts_merged[!is_leap_year(year) & doy > feb28, doy := doy + 1]
    ts_merged[, year := NULL]
    data.table::setcolorder(ts_merged, c("doy", "ts_x", "ts_y"))
  }

  return(ts_merged)
}
