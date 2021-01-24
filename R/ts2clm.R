#' Make a climatology from a daily time series.
#'
#' Creates a daily climatology from a time series of daily temperatures using a
#' user-specified sliding window for the mean and threshold calculation, followed
#' by an optional moving average smoother as used by Hobday et al. (2016).
#'
#' @importFrom data.table %between%
#' @useDynLib heatwaveR
#'
#' @param data A data frame with two columns. In the default setting (i.e. omitting
#' the arguments \code{x} and \code{y}; see immediately below), the data set is
#' expected to have the headers \code{t} and \code{temp}. The \code{t} column is a
#' vector of dates of class \code{Date}, while \code{temp} is the measured variable
#' (by default it is assumed to be temperature).
#' @param x This column is expected to contain a vector of dates. If a column
#' headed \code{t} is present in the dataframe, this argument may be omitted;
#' otherwise, specify the name of the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param climatologyPeriod Required. To this argument should be passed two values
#' (see example below). The first value should be the chosen date for the start of
#' the climatology period, and the second value the end date of said period. This
#' chosen period (preferably 30 years in length) is then used to calculate the
#' seasonal cycle and the extreme value threshold.
#' @param robust This argument has been deprecated and no longer has affects how
#' the function operates.
#' @param maxPadLength Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{maxPadLength} will be left as \code{NA}. The default is
#' \code{FALSE}. Set as an integer to interpolate. Setting \code{maxPadLength}
#' to \code{TRUE} will return an error.
#' @param windowHalfWidth Width of sliding window about day-of-year (to one
#' side of the center day-of-year) used for the pooling of values and
#' calculation of climatology and threshold percentile. Default is \code{5}
#' days, which gives a window width of 11 days centred on the 6th day of the
#' series of 11 days.
#' @param pctile Threshold percentile (\%) for detection of events (MHWs).
#' Default is \code{90}th percentile. Should the intent be to use these
#' threshold data for MCSs, set \code{pctile = 10}. Or some other low value.
#' @param smoothPercentile Boolean switch selecting whether to smooth the
#' climatology and threshold percentile time series with a moving average of
#' \code{smoothPercentileWidth}. Default is \code{TRUE}.
#' @param smoothPercentileWidth Full width of moving average window for smoothing
#' climatology and threshold. The default is \code{31} days.
#' @param clmOnly Choose to calculate and return only the climatologies.
#' The default is \code{FALSE}.
#' @param var This argument has been introduced to allow the user to choose if
#' the variance of the seasonal signal per doy should be calculated. The default of
#' \code{FALSE} will prevent the calculation, potentially increasing speed of calculations
#' on gridded data and reducing the size of the output. The variance was initially
#' introduced as part of the standard output from Hobday et al. (2016), but few
#' researchers use it and so it is generally regarded now as unnecessary.
#' @param roundClm This argument allows the user to choose how many decimal places
#' the \code{seas} and \code{thresh} outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundClm = FALSE}. This argument may only be given
#' numeric values or FALSE.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily values with few missing values. Time ranges which start and end
#' part-way through the calendar year are supported.
#' \item It is recommended that a period of at least 30 years is specified in
#' order to produce a climatology that smooths out any decadal thermal
#' periodicities that may be present. It is further advised that full the start
#' and end dates for the climatology period result in full years, e.g.
#' "1982-01-01" to "2011-12-31" or "1982-07-01" to "2012-06-30"; if not, this
#' may result in an unequal weighting of data belonging with certain months
#' within a time series. A daily climatology will be created; that is, the
#' climatology will be comprised of one mean temperature for each day of the
#' year (365 or 366 days, depending on how leap years are dealt with), and the
#' mean will be based on a sample size that is a function of the length of time
#' determined by the start and end values given to \code{climatologyPeriod} and
#' the width of the sliding window specified in \code{windowHalfWidth}.
#' \item This function supports leap years. This is done by ignoring Feb 29s
#' for the initial calculation of the climatology and threshold. The values for
#' Feb 29 are then linearly interpolated from the values for Feb 28 and Mar 1.
#' \item Previous versions of \code{ts2clm()} tested to see if some rows
#' are duplicated, or if replicate temperature readings are present per day, but
#' this has now been disabled. Should the user be concerned about such repeated
#' measurements, we suggest that the necessary checks and fixes are implemented
#' prior to feeding the time series to \code{ts2clm()}.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016).
#'
#' @return The function will return a tibble (see the \code{tidyverse}) with the
#' input time series and the newly calculated climatology. The climatology contains
#' the seasonal climatology and the threshold for calculating MHWs. The software was
#' designed for creating climatologies of daily temperatures, and the units
#' specified below reflect that intended purpose. However, various other kinds
#' of climatologies may be created, and if that is the case, the appropriate
#' units need to be determined by the user.
#'   \item{doy}{Julian day (day-of-year). For non-leap years it runs 1...59 and
#'   61...366, while leap years run 1...366.}
#'   \item{t}{The date vector in the original time series supplied in \code{data}. If
#'   an alternate column was provided to the \code{x} argument, that name will rather
#'   be used for this column.}
#'   \item{temp}{The measurement vector as per the the original \code{data} supplied
#'   to the function. If a different column was given to the \code{y} argument that
#'   will be shown here.}
#'   \item{seas}{Climatological seasonal cycle [deg. C].}
#'   \item{thresh}{Seasonally varying threshold (e.g., 90th
#'   percentile) [deg. C]. This is used in \code{\link{detect_event}} for the
#'   detection/calculation of events (MHWs).}
#'   \item{var}{Seasonally varying variance (standard deviation) [deg. C]. This
#'   column is not returned if \code{var = FALSE} (default).}
#' Should \code{clmOnly} be enabled, only the 365 or 366 day climatology will be
#' returned.
#'
#' @author Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016). A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi:10.1016/j.pocean.2015.12.014
#'
#' @export
#'
#' @examples
#' res <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res[1:10, ]
#'
#' # Or if one only wants the 366 day climatology
#' res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
#'                    clmOnly = TRUE)
#' res_clim[1:10, ]
#'
#' # Or if one wants the variance column included in the results
#' res_var <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
#'                   var = TRUE)
#' res_var[1:10, ]
#'
ts2clm <- function(data,
                   x = t,
                   y = temp,
                   climatologyPeriod,
                   robust = FALSE,
                   maxPadLength = FALSE,
                   windowHalfWidth = 5,
                   pctile = 90,
                   smoothPercentile = TRUE,
                   smoothPercentileWidth = 31,
                   clmOnly = FALSE,
                   var = FALSE,
                   roundClm = 4) {

    if (missing(climatologyPeriod))
      stop("Oops! Please provide a period (two dates) for calculating the climatology.")
    if (length(climatologyPeriod) != 2)
      stop("Bummer! Please provide BOTH start and end dates for the climatology period.")
    if (!(is.logical(robust)))
      stop("Please ensure that 'robust' is either TRUE or FALSE.")
    if (robust)
      message("The 'robust' argument has been deprecated and will be removed from future versions.")
    if (maxPadLength != FALSE & !is.numeric(maxPadLength))
      stop("Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")
    if (!(is.numeric(pctile)))
      stop("Please ensure that 'pctile' is a numeric/integer value.")
    if (!(is.numeric(windowHalfWidth)))
      stop("Please ensure that 'windowHalfWidth' is a numeric/integer value.")
    if (!(is.logical(smoothPercentile)))
      stop("Please ensure that 'smoothPercentile' is either TRUE or FALSE.")
    if (!(is.numeric(smoothPercentileWidth)))
      stop("Please ensure that 'smoothPercentileWidth' is a numeric/integer value.")
    if (!(is.logical(clmOnly)))
      stop("Please ensure that 'clmOnly' is either TRUE or FALSE.")
    if (!(is.numeric(roundClm))) {
      if (!roundClm == FALSE) {
        stop("Please ensure that 'roundClm' is either a numeric value or FALSE.")
      }
    }

    clim_start <- climatologyPeriod[1]
    clim_end <- climatologyPeriod[2]
    temp <- doy <- .SD <-  NULL

    ts_x <- eval(substitute(x), data)
    ts_y <- eval(substitute(y), data)
    rm(data)

    if (!inherits(ts_x[1], "Date"))
      stop("Please ensure your date values are type 'Date'. This may be done with 'as.Date()'.")
    if (!is.numeric(ts_y[1]))
      stop("Please ensure the temperature values you are providing are type 'num' for numeric.")

    ts_xy <- data.table::data.table(ts_x = ts_x, ts_y = ts_y)[base::order(ts_x)]
    rm(ts_x); rm(ts_y)

    ts_whole <- make_whole_fast(ts_xy)

    if (length(stats::na.omit(ts_whole$ts_y)) < length(ts_whole$ts_y) & is.numeric(maxPadLength)) {
      ts_whole <- na_interp(doy = ts_whole$doy,
                            x = ts_whole$ts_x,
                            y = ts_whole$ts_y,
                            maxPadLength = maxPadLength)
    }

    if (ts_whole$ts_x[1] > clim_start)
      stop(paste("The specified start date precedes the first day of series, which is",
                 ts_whole$ts_x[1]))

    if (clim_end > utils::tail(ts_whole$ts_x, 1))
      stop(paste("The specified end date follows the last day of series, which is",
                 ts_whole$ts_x[nrow(ts_whole)]))

    if (as.Date(clim_end) - as.Date(clim_start) < 1095)
      stop("The climatologyPeriod must be at least three years to calculate thresholds")

    ts_wide <- clim_spread(ts_whole, clim_start, clim_end, windowHalfWidth)

    if (nrow(stats::na.omit(ts_wide)) < nrow(ts_wide) | var) {
      ts_mat <- clim_calc(ts_wide, windowHalfWidth, pctile)
      ts_mat[is.nan(ts_mat)] <- NA
    } else {
      ts_mat <- clim_calc_cpp(ts_wide, windowHalfWidth, pctile)
    }
    rm(ts_wide)

    if (smoothPercentile) {
      ts_clim <- smooth_percentile(ts_mat, smoothPercentileWidth, var)
    } else {
      ts_clim <- data.table::data.table(ts_mat)
    }

    cols <- names(ts_clim)
    if (is.numeric(roundClm)) {
      ts_clim[,(cols) := round(.SD, roundClm), .SDcols = cols]
    }
    rm(ts_mat)

    if (clmOnly) {

      return(ts_clim)

    } else {

      data.table::setkey(ts_whole, doy)
      data.table::setkey(ts_clim, doy)
      ts_res <- merge(ts_whole, ts_clim, all = TRUE)
      rm(ts_whole); rm(ts_clim)
      data.table::setorder(ts_res, ts_x)
      names(ts_res)[2] <- paste(substitute(x))
      names(ts_res)[3] <- paste(substitute(y))
      ts_res <- tibble::as_tibble(ts_res)
      return(ts_res)

    }
  }
