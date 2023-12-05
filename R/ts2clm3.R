#' Make a climatology from a daily time series.
#'
#' This is the fully data.table-based version of \code{\link{ts2clm}}. The function creates
#' a daily climatology from a time series of daily temperatures using a
#' user-specified sliding window for the mean and threshold calculation, followed
#' by an optional moving average smoother as used by Hobday et al. (2016).
#'
#' @import Rcpp
#'
#' @importFrom data.table .N
#' @useDynLib heatwaveR
#'
#' @param data A data frame with at least two columns. In the default setting
#' (i.e. omitting the arguments \code{x} and \code{y}; see immediately below),
#' the data set is expected to have the headers \code{t} and \code{temp}. The
#' \code{t} column is a vector of dates of class \code{Date}, while \code{temp}
#' is the measured variable (by default it is assumed to be temperature). Any
#' additional columns are not used in calculations but will be correctly
#' carried over into the output climatology. Such additional columns may be
#' site names, longitudes, or latitudes (etc.), but note that they are not meant
#' to specify a grouping structure of the time series. As such, grouping structures
#' are not handled by the function in its current form, and a time series is
#' therefore assumed to be located at a discrete point in space such as one
#' 'pixel' of longitude × latitude as one might find in gridded data products.
#' @param x This column is expected to contain a vector of dates. If a column
#' headed \code{t} is present in the data frame, this argument may be omitted;
#' otherwise, specify the name of the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param climatologyPeriod Required. To this argument should be passed two values
#' (see example below). The first value should be the chosen date for the start of
#' the climatology period, and the second value the end date of said period. This
#' chosen period (preferably 30 years in length) is then used to calculate the
#' seasonal cycle and the extreme value threshold.
#' @param maxPadLength Specifies the maximum length of days over which to apply
#' linear interpolation (padding) across the missing values (specified as \code{NA})
#' in the measured variable; i.e., any consecutive blocks of NAs with length
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
#' threshold data for MCSs, set \code{pctile = 10} or some other low value.
#' @param smoothPercentile Boolean. Select whether to smooth the climatology
#' and threshold percentile time series with a moving average of
#' \code{smoothPercentileWidth}. The default is \code{TRUE}.
#' @param smoothPercentileWidth Full width of moving average window for smoothing
#' the climatology and threshold. The default is \code{31} days.
#' @param clmOnly Boolean. Choose to calculate and return only the climatologies.
#' The default is \code{FALSE}.
#' @param var Boolean. This argument has been introduced to allow the user to
#' choose if the variance of the seasonal signal per doy should be calculated.
#' The default of \code{FALSE} will prevent the calculation. Setting it to
#' \code{TRUE} might potentially increase the speed of calculations on gridded
#' data and increase the size of the output. The variance was initially introduced
#' as part of the standard output from Hobday et al. (2016), but few researchers
#' use it and so it is generally regarded now as unnecessary.
#' @param roundClm This argument allows the user to choose how many decimal places
#' the \code{seas} and \code{thresh} outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundClm = FALSE}. This argument may only be given
#' numeric values or FALSE.
#' @param ... Allows unused arguments to pass through the functions.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily values with few missing values. Time ranges which start and end
#' part-way through the calendar year are supported.
#' \item It is recommended that a period of at least 30 years is specified in
#' order to produce a climatology that smooths out any decadal thermal
#' periodicities that may be present. When calculated over at least 30 years of
#' data, such a climatology is called a 'climatological normal.' It is further
#' advised that full the start and end dates for the climatology period result
#' in full years, e.g. "1982-01-01" to "2011-12-31" or "1982-07-01" to
#' "2012-06-30"; if not, this may result in an unequal weighting of data
#' belonging with certain months within a time series. A daily climatology will
#' be created; that is, the climatology will be comprised of one mean
#' temperature for each day of the year (365 or 366 days, depending on how
#' leap years are dealt with), and the mean will be based on a sample size that
#' is a function of the length of time determined by the start and end values
#' given to \code{climatologyPeriod} and the width of the sliding window
#' specified in \code{windowHalfWidth}.
#' \item This function supports leap years. This is done by ignoring Feb 29s
#' for the initial calculation of the climatology and threshold. The values
#' for Feb 29 are then linearly interpolated from the values for Feb 28 and
#' Mar 1.
#' \item Previous versions of \code{ts2clm()} tested to see if some rows are
#' duplicated, or if replicate temperature readings are present per day, but
#' this has now been disabled. Should the user be concerned about such repeated
#' measurements, we suggest that the necessary checks and fixes are implemented
#' prior to feeding the time series to \code{ts2clm()}.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016).
#'
#' @return The function will return a data.table (see the \code{data.table}) with the
#' input time series and the newly calculated climatology. The climatology contains
#' the daily climatology and the threshold for calculating MHWs. The software was
#' designed for creating climatologies of daily temperatures, and the units
#' specified below reflect that intended purpose. However, various other kinds
#' of climatologies may be created, and if that is the case, the appropriate
#' units need to be determined by the user.
#'   \item{doy}{Julian day (day-of-year) returned when \code{clmOnly = TRUE}. For non-leap
#'   years it runs 1...59 and 61...366, while leap years run 1...366.}
#'   \item{t}{The date vector in the original time series supplied in \code{data}. If
#'   an alternate column was provided to the \code{x} argument, that name will rather
#'   be used for this column.}
#'   \item{temp}{The measurement vector as per the the original \code{data} supplied
#'   to the function. If a different column was given to the \code{y} argument that
#'   will be shown here.}
#'   \item{seas}{Daily climatological cycle [deg. C].}
#'   \item{thresh}{Daily varying threshold (e.g., 90th
#'   percentile) [deg. C]. This is used in \code{\link{detect_event3}} for the
#'   detection/calculation of events (MHWs).}
#'   \item{var}{Daily varying variance (standard deviation) [deg. C]. This
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
#' data.table::setDTthreads(threads = 1) # optimise for your code and local computer
#' res <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res[1:10, ]
#'
#' # Or if one only wants the 366 day climatology
#' res_clim <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
#'                     clmOnly = TRUE)
#' res_clim[1:10, ]
#'
#' # Or if one wants the variance column included in the results
#' res_var <- ts2clm3(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
#'                    var = TRUE)
#' res_var[1:10, ]
#'
ts2clm3 <- function(data,
                    x = t,
                    y = temp,
                    climatologyPeriod,
                    maxPadLength = FALSE,
                    windowHalfWidth = 5,
                    pctile = 90,
                    smoothPercentile = TRUE,
                    smoothPercentileWidth = 31,
                    clmOnly = FALSE,
                    var = FALSE,
                    roundClm = 4,
                    ...) {
  if (missing(climatologyPeriod))
    stop("Oops! Please provide a period (two dates) for calculating the climatology.")

  if (length(climatologyPeriod) != 2)
    stop("Bummer! Please provide BOTH start and end dates for the climatology period.")

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

  data.table::setDT(data)

  clim_start <- fasttime::fastDate(climatologyPeriod[1])
  clim_end <- fasttime::fastDate(climatologyPeriod[2])

  mean_ts_y <- temp <- seas <- thresh <- .SD <-  NULL
  year <- doy <- ts_x <- NULL
  seas_roll <- thresh_roll <- thresh_var_roll <- NULL

  ts_x <- eval(substitute(x), data)
  if (is.null(ts_x) | is.function(ts_x))
    stop(
      "Please ensure that a column named 't' is present in your data.table or that you have assigned a column to the 'x' argument."
    )

  ts_y <- eval(substitute(y), data)
  if (is.null(ts_y) | is.function(ts_y))
    stop(
      "Please ensure that a column named 'temp' is present in your data.table or that you have assigned a column to the 'y' argument."
    )

  if (!inherits(ts_x[1], "Date"))
    stop("Please ensure your date values are type 'Date'. This may be done with 'as.Date()'.")

  if (!is.numeric(ts_y[1]))
    stop("Please ensure the temperature values you are providing are type 'num' for numeric.")

  ts_xy <-
    data.table::data.table(ts_x = ts_x, ts_y = ts_y)[base::order(ts_x)]
  rm(list = c("ts_x", "ts_y"))

  # BEGIN INSERT make_whole_fast >>>

  ts_full <- data.table::data.table(ts_x = seq.Date(ts_xy[1, ts_x],
                                                    ts_xy[.N, ts_x],
                                                    "day"))

  feb28 <- 59

  .is_leap_year <- function(year) {
    return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
  }

  ts_whole <-
    data.table::merge.data.table(ts_full,
                                 ts_xy,
                                 by = "ts_x",
                                 all.x = TRUE)[,
                                               `:=`(year = as.integer(format(ts_x, "%Y")),
                                                    doy = as.integer(format(ts_x, "%j")))][,
                                                                                           doy := ifelse(!.is_leap_year(year) &
                                                                                                           doy > 59, doy + 1, doy)][, year := NULL][, data.table::setcolorder(.SD, c("doy", "ts_x", "ts_y"))]
  data.table::setkey(ts_whole, NULL)

  rm(ts_full)

  # END INSERT make_whole_fast <<<

  # BEGIN INSERT na_interp >>>

  if (sum(stats::complete.cases(ts_whole$ts_y)) < nrow(ts_whole) &
      is.numeric(maxPadLength)) {

    .na_pad <- function(x, fill, maxPadLength) {
      if (maxPadLength <= 0)
        return(x)
      if (maxPadLength >= length(x))
        return(fill)
      naruns <- rle(is.na(x))
      naruns$values[naruns$lengths > maxPadLength] <- FALSE
      naok <- inverse.rle(naruns)
      rm(naruns)
      ifelse(naok, fill, x)
    }

    .na_fun <- function(x, y) {
      na <- is.na(y)
      yi <- stats::approx(x[!na], y[!na], x)$y
      ygap <-
        .na_pad(y, seq_along(y), maxPadLength = maxPadLength)
      ix <- stats::approx(x, seq_along(y), x)$y
      yx <-
        ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]), NA, yi)
      yx
    }

    ts_whole[, ts_y := .na_fun(ts_x, ts_y)]

    # END INSERT na_interp <<<
  }

  if (ts_whole$ts_x[1] > clim_start)
    stop(
      paste(
        "The specified start date precedes the first day of series, which is",
        ts_whole$ts_x[1]
      )
    )

  if (clim_end > utils::tail(ts_whole$ts_x, 1))
    stop(paste(
      "The specified end date follows the last day of series, which is",
      ts_whole$ts_x[nrow(ts_whole)]
    ))

  if (clim_end - clim_start < 1095)
    stop("The climatologyPeriod must be at least three years to calculate thresholds")

  # BEGIN INSERT clim_spread >>>

  .NA2mean <- function(x) {
    z <- round(mean(x, na.rm = TRUE), 2)
    x[is.na(x)] <- z

    return(x)
  }

  ts_clim <- ts_whole[ts_x >= clim_start & ts_x <= clim_end,
                      list(ts_x = format(ts_x, "%Y"), doy, ts_y)]

  ts_clim[, mean_ts_y := mean(ts_y, na.rm = TRUE), by = list(doy, ts_x)]
  ts_spread <-
    data.table::dcast(ts_clim, doy ~ ts_x, value.var = "mean_ts_y")
  rm(ts_clim)

  ts_spread_filled <-
    data.table::data.table((sapply(ts_spread[59:61,],
                                   function(x)
                                     .NA2mean(x))))

  ts_spread[60, (names(ts_spread)) := ts_spread_filled[2, .SD, .SDcols = names(ts_spread)]]
  rm(ts_spread_filled)

  l <- list(ts_spread[(.N - windowHalfWidth + 1):.N,],
            ts_spread,
            ts_spread[1:windowHalfWidth,])
  ts_spread <- data.table::rbindlist(l)
  rm(l)

  len_yr <-
    length(as.integer(substr(clim_start, 1, 4)):as.integer(substr(clim_end, 1, 4)))

  ts_mat <- as.matrix(ts_spread)[, 2:(len_yr + 1)]

  if (nrow(stats::na.omit(ts_mat)) < nrow(ts_mat)) {
    plugs <- which(is.na(ts_mat), arr.ind = TRUE)
    ts_mat[plugs] <- rowMeans(ts_mat, na.rm = TRUE)[plugs[, 1]]
  }

  # END INSERT clim_spread <<<

  if (nrow(stats::na.omit(ts_mat)) < nrow(ts_mat) | var) {
    ts_mat <- clim_calc(ts_mat, windowHalfWidth, pctile)
    ts_mat[is.nan(ts_mat)] <- NA

  } else {
    ts_mat <- clim_calc_cpp(ts_mat, windowHalfWidth, pctile)
    ts_mat <- data.table::data.table(ts_mat)
  }

  # BEGIN INSERT smooth_percentile >>>

  if (smoothPercentile) {

    prep <-
      data.table::rbindlist(
        list(
          data.table::data.table(utils::tail(ts_mat, smoothPercentileWidth)),
          data.table::data.table(ts_mat),
          data.table::data.table(utils::head(ts_mat, smoothPercentileWidth))
        )
      )

    prep[, `:=`(
      doy = doy,
      seas_roll = data.table::frollmean(
        seas,
        n = smoothPercentileWidth,
        na.rm = FALSE,
        align = "center"
      ),
      thresh_roll = data.table::frollmean(
        thresh,
        n = smoothPercentileWidth,
        na.rm = FALSE,
        align = "center"
      )
    )]
    ts_clim <-
      prep[(smoothPercentileWidth + 1):(nrow(prep) - smoothPercentileWidth),
           list(doy = doy,
                seas = seas_roll,
                thresh = thresh_roll)]

    if (var) {
      prep <-
        data.table::rbindlist(
          list(
            data.table::data.table(utils::tail(ts_mat, smoothPercentileWidth)),
            data.table::data.table(ts_mat),
            data.table::data.table(utils::head(ts_mat, smoothPercentileWidth))
          )
        )

      prep[, `:=`(
        doy = doy,
        seas_roll = data.table::frollmean(
          seas,
          n = smoothPercentileWidth,
          na.rm = FALSE,
          align = "center"
        ),
        thresh_roll = data.table::frollmean(
          thresh,
          n = smoothPercentileWidth,
          na.rm = FALSE,
          align = "center"
        ),
        thresh_var_roll = data.table::frollapply(
          thresh,
          n = smoothPercentileWidth,
          var,
          na.rm = FALSE,
          align = "center"
        )
      )]

      ts_clim <-
        prep[(smoothPercentileWidth + 1):(nrow(prep) - smoothPercentileWidth),
             list(
               doy = doy,
               seas = seas_roll,
               thresh = thresh_roll,
               var = thresh_var_roll
             )]
    }
    rm(prep)

    # END INSERT smooth_percentile <<<

  } else {
    ts_clim <- ts_mat
  }

  if (is.numeric(roundClm)) {
    ts_clim[, (names(ts_clim)) := lapply(.SD, function(x)
      round(x, digits = roundClm))]
  }
  rm(ts_mat)

  if (clmOnly) {

    return(ts_clim)

  } else {
    ts_res <-
      ts_whole[ts_clim, on = list(doy), nomatch = 0][order(ts_x)]
    rm(ts_whole)
    rm(ts_clim)
    names(ts_res)[2] <- paste(substitute(x))
    names(ts_res)[3] <- paste(substitute(y))

    # Implement a data.table method
    if (ncol(data) > 2) {
      merge_cols <- colnames(ts_res)[2:3]
      ts_res <-
        data.table::merge.data.table(data, ts_res, by = merge_cols, all = TRUE)
    }

    ts_res[, doy := NULL]

    return(ts_res)
  }
}
