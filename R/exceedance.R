#' Detect consecutive days in exceedance above or below of a given threshold.
#'
#' @param data A data frame with at least the two following columns:
#' a \code{t} column which is a vector of dates of class \code{Date},
#' and a \code{temp} column, which is the temperature on those given
#' dates. If columns are named differently, their names can be supplied as \code{x}
#' and \code{y} (see below). The function will not accurately detect consecutive
#' days of temperatures in exceedance of the \code{threshold} if missing days of
#' data are not filled in with \code{NA}. Data of the appropriate format are created
#' by the internal function \code{\link{make_whole_fast}}, but your own data may be used
#' directly if they meet the given criteria. Note that it is also possible to provide hourly
#' data in the \code{x} column as class \code{POSIXct}.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{make_whole_fast}. If a column headed \code{t} is present in
#' the dataframe, this argument may be omitted; otherwise, specify the name of
#' the column with dates here. Note that it is also possible to provide hourly
#' data as class \code{POSIXct}.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param threshold The static threshold used to determine how many consecutive
#' days are in exceedance of the temperature of interest.
#' @param below Default is \code{FALSE}. When set to TRUE, consecutive days of temperature
#' below the \code{threshold} variable are calculated. When set to FALSE,
#' consecutive days above the \code{threshold} variable are calculated.
#' @param minDuration Minimum duration that temperatures must be in exceedance
#' of the \code{threshold} variable. The default is \code{5} days.
#' @param joinAcrossGaps A TRUE/FALSE statement that indicates whether
#' or not to join consecutive days of temperatures in exceedance of the
#' \code{threshold} across a small gap between groups before/after a short
#' gap as specified by \code{maxGap}. The default is \code{TRUE}.
#' @param maxGap The maximum length of the gap across which to connect
#' consecutive days in exceedance of the \code{threshold} when
#' \code{joinAcrossGaps = TRUE}.
#' @param maxPadLength Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{maxPadLength} will be left as \code{NA}. Set as an
#' integer. The default is \code{3} days. Note this will be units of hours if
#' hourly data were provided.
#' @param roundRes This argument allows the user to choose how many decimal places
#' the exceedance metric outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundRes = FALSE}. This argument may only be given
#' numeric values or FALSE.
#' @param returnDF The default (\code{TRUE}) tells the function to return the results as
#' type \code{data.frame}. \code{FALSE} will return the results as a \code{data.table}.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily temperatures, with few missing values. The accompanying function
#' \code{\link{make_whole_fast}} aids in the preparation of a time series that is
#' suitable for use with \code{exceedance}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{make_whole_fast}}.
#' \item Future versions seek to accommodate monthly and annual time series, too.
#' \item The calculation of onset and decline rates assumes that exceedance of the
#' \code{threshold} started a half-day before the start day and ended a half-day
#' after the end-day. This is consistent with the duration definition as implemented,
#' which assumes duration = end day - start day + 1.
#' \item For the purposes of exceedance detection, any missing temperature values not
#' interpolated over (through optional \code{maxPadLength}) will remain as
#' \code{NA}. This means they will trigger the end of an exceedance if the adjacent
#' temperature values are in exceedance of the \code{threshold}.
#' \item If the function is used to detect consecutive days of temperature under
#' the given \code{theshold}, these temperatures are then taken as being in
#' exceedance below the \code{threshold} as there is no antonym in the English
#' language for 'exceedance'.
#' }
#' This function is based largely on the \code{detect_event} function found in this
#' package, which was ported from the Python algorithm that was written by Eric
#' Oliver, Institute for Marine and Antarctic Studies, University of Tasmania,
#' Feb 2015, and is documented by Hobday et al. (2016).
#'
#' @return The function will return a list of two data.frames.
#' The first being \code{threshold}, which shows the daily temperatures and on which
#' specific days the given \code{threshold} was exceeded. The second component of the
#' list is \code{exceedance}, which shows a medley of statistics for each discrete
#' group of days in exceedance of the given \code{threshold}. Note that any additional
#' columns left in the data frame given to this function will be output in the
#' \code{threshold} component of the output. For example, if one uses
#' \code{\link{ts2clm}} to prepare a time series for analysis and leaves
#' in the \code{doy} column, this column will appear in the output.
#'
#' The information shown in the \code{threshold} component is:
#'   \item{t}{The date of the temperature measurement. This variable may named
#'   differently if an alternative name is supplied to the function's \code{x}
#'   argument.}
#'   \item{temp}{Temperature on the specified date [deg. C]. This variable may
#'   named differently if an alternative name is supplied to the function's \code{y}
#'   argument.}
#'   \item{thresh}{The static \code{threshold} chosen by the user [deg. C].}
#'   \item{thresh_criterion}{Boolean indicating if \code{temp} exceeds
#'   \code{threshold}.}
#'   \item{duration_criterion}{Boolean indicating whether periods of consecutive
#'   \code{thresh_criterion} are >= \code{minDuration}.}
#'   \item{exceedance}{Boolean indicting if all criteria that define a discrete
#'   group in exceedance of the \code{threshold} are met.}
#'   \item{exceedance_no}{A sequential number indicating the ID and order of
#'   occurrence of exceedances.}
#'
#' The individual exceedances are summarised using the following metrics:
#'   \item{exceedance_no}{The same sequential number indicating the ID and
#'   order of the exceedance as found in the \code{threshold} component of the
#'   output list.}
#'   \item{index_start}{Row number on which exceedance starts.}
#'   \item{index_peak}{Row number on which exceedance peaks.}
#'   \item{index_end}{Row number on which exceedance ends.}
#'   \item{duration}{Duration of exceedance [days].}
#'   \item{date_start}{Start date of exceedance [date].}
#'   \item{date_peak}{Date of exceedance peak [date].}
#'   \item{date_end}{End date of exceedance [date].}
#'   \item{intensity_mean}{Mean intensity [deg. C].}
#'   \item{intensity_max}{Maximum (peak) intensity [deg. C].}
#'   \item{intensity_var}{Intensity standard deviation [deg. C].}
#'   \item{intensity_cumulative}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of exceedance [deg. C / day].}
#'   \item{rate_decline}{Decline rate of exceedance [deg. C / day].}
#'
#' \code{intensity_max_abs}, \code{intensity_mean_abs}, \code{intensity_var_abs},
#' and \code{intensity_cum_abs} are as above except as absolute magnitudes rather
#' than relative to the threshold.
#'
#' @author Robert W. Schlegel, Albertus J. Smit
#'
#' @export
#'
#' @examples
#' res <- exceedance(sst_WA, threshold = 25)
#' # show first ten days of daily data:
#' res$threshold[1:10, ]
#' # show first five exceedances:
#' res$exceedance[1:5, ]
#'
exceedance <- function(data,
                       x = t,
                       y = temp,
                       threshold,
                       below = FALSE,
                       minDuration = 5,
                       joinAcrossGaps = TRUE,
                       maxGap = 2,
                       maxPadLength = FALSE,
                       roundRes = 4,
                       returnDF = TRUE) {

  temp <- threshCriterion <- durationCriterion <- event <- event_no <- doy <- NULL

  ts_x <- eval(substitute(x), data)
  if (is.null(ts_x) | is.function(ts_x))
    stop("Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  ts_y <- eval(substitute(y), data)
  if (is.null(ts_y) | is.function(ts_y))
    stop("Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
  if (inherits(ts_x[1], "POSIXct")){
    ts_x_hourly <- round(ts_x, units = "hours")
    if (any(!ts_x == ts_x_hourly))
      stop("Please ensure that timesteps are even hours.
           E.g. data$x <- round(data$x, units = 'hours')")
  }

  # ts_xy <- data.frame(ts_x = ts_Med_hourly$t, ts_y = ts_Med_hourly$temp)
  ts_xy <- data.frame(ts_x = ts_x, ts_y = ts_y)
  rm(ts_x, ts_y)

  ts_whole <- make_whole_fast(ts_xy)

  if (length(stats::na.omit(ts_whole$ts_y)) < length(ts_whole$ts_y) & is.numeric(maxPadLength)) {
    ts_whole <- na_interp(doy = ts_whole$doy,
                          x = ts_whole$ts_x,
                          y = ts_whole$ts_y,
                          maxPadLength = maxPadLength)
  }

  if (missing(threshold))
    stop("Oh no! Please provide a threshold against which to calculate exceedances.")
  if (maxPadLength != FALSE & !is.numeric(maxPadLength))
    stop("Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")

  if (below) {
    ts_whole$ts_y <- -ts_whole$ts_y
    threshold <- -threshold
  }

  ts_whole$thresh <- rep(threshold, nrow(ts_whole))
  ts_whole$threshCriterion <- ts_whole$ts_y > ts_whole$thresh
  ts_whole$threshCriterion[is.na(ts_whole$threshCriterion)] <- FALSE

  exceedances_clim <- proto_event(ts_whole,
                                  criterion_column = ts_whole$threshCriterion,
                                  minDuration = minDuration,
                                  joinAcrossGaps = joinAcrossGaps,
                                  maxGap = maxGap)

  if("hoy" %in% colnames(exceedances_clim)) {
    exceedances_clim <- exceedances_clim[,-c(1, 2)]
  } else {
    exceedances_clim <- exceedances_clim[,-1]
  }

  colnames(exceedances_clim)[c(6,7)] <- c("exceedance", "exceedance_no")

  thresh <- intensity_mean <- intensity_max <- intensity_cumulative <-
    exceedance_rel_thresh <- intensity_mean_abs <- intensity_max_abs <-
    intensity_cum_abs <- ts_y <- exceedance_no <- row_index <-
    index_start <- index_peak <- index_end <- NULL

  if (nrow(stats::na.omit(exceedances_clim)) > 0) {

    exceedances <- exceedances_clim
    exceedances$row_index <- seq_len(nrow(exceedances_clim))
    exceedances$exceedance_rel_thresh <- exceedances$ts_y - exceedances$thresh
    exceedances <- exceedances[stats::complete.cases(exceedances$exceedance_no)]
    exceedances <- data.table::setDT(exceedances)

    exceedances <- exceedances[, list(
      index_start = min(row_index),
      index_peak = row_index[which.max(exceedance_rel_thresh)][1],
      index_end = max(row_index),
      duration = max(row_index) - min(row_index) + 1,
      date_start = min(ts_x),
      date_peak = ts_x[which.max(exceedance_rel_thresh)][1],
      date_end = max(ts_x),
      intensity_mean = mean(exceedance_rel_thresh),
      intensity_max = max(exceedance_rel_thresh),
      intensity_var = stats::sd(exceedance_rel_thresh),
      intensity_cumulative = sum(exceedance_rel_thresh),
      intensity_mean_abs = mean(ts_y),
      intensity_max_abs = max(ts_y),
      intensity_var_abs = stats::sd(ts_y),
      intensity_cum_abs = sum(ts_y)
    ), by = list(exceedance_no)]

    exceedance_rel_thresh <- ts_whole$ts_y - ts_whole$thresh
    A <- exceedance_rel_thresh[exceedances$index_start]
    B <- ts_whole$ts_y[exceedances$index_start - 1]
    C <- ts_whole$thresh[exceedances$index_start - 1]
    if (length(B) + 1 == length(A)) {
      B <- c(NA, B)
      C <- c(NA, C)
    }
    exceedance_rel_thresh_start <- 0.5 * (A + B - C)

    exceedances$rate_onset <- base::ifelse(
      exceedances$index_start > 1,
      (exceedances$intensity_max - exceedance_rel_thresh_start) / (as.numeric(
        difftime(exceedances$date_peak, exceedances$date_start, units = "days")) + 0.5),
      NA
    )

    D <- exceedance_rel_thresh[exceedances$index_end]
    E <- ts_whole$ts_y[exceedances$index_end + 1]
    G <- ts_whole$thresh[exceedances$index_end + 1]
    exceedance_rel_thresh_end <- 0.5 * (D + E - G)

    exceedances$rate_decline <- base::ifelse(
      exceedances$index_end < nrow(ts_whole),
      (exceedances$intensity_max - exceedance_rel_thresh_end) / (as.numeric(
        difftime(exceedances$date_end, exceedances$date_peak, units = "days")) + 0.5),
      NA
    )

    if (below) {
      exceedances$intensity_mean <- -exceedances$intensity_mean
      exceedances$intensity_max <- -exceedances$intensity_max
      exceedances$intensity_cumulative <- -exceedances$intensity_cumulative
      exceedances$intensity_mean_abs <- -exceedances$intensity_mean_abs
      exceedances$intensity_max_abs <- -exceedances$intensity_max_abs
      exceedances$intensity_cum_abs <- -exceedances$intensity_cum_abs
    }

  } else {
    exceedances <- data.frame(exceedance_no = NA, index_start = NA, index_peak = NA, index_end = NA,
                              duration = NA, date_start = NA, date_peak = NA, date_end = NA,
                              intensity_mean = NA, intensity_max = NA, intensity_var = NA,
                              intensity_cumulative = NA, intensity_mean_abs = NA,
                              intensity_max_abs = NA, intensity_var_abs = NA,
                              intensity_cum_abs = NA, rate_onset = NA, rate_decline = NA)
  }
  exceedances[,9:18] <- round(exceedances[,9:18], roundRes)

  if (below) {
    exceedances_clim$ts_y <- -exceedances_clim$ts_y
    exceedances_clim$thresh <- -exceedances_clim$thresh
  }

  data_merge <- as.data.frame(data)[,!(colnames(data) == paste(substitute(y))), drop = FALSE]
  exceedances_merge <- as.data.frame(exceedances_clim)

  names(exceedances_merge)[1] <- paste(substitute(x))
  names(exceedances_merge)[2] <- paste(substitute(y))
  clim_names <- colnames(exceedances_merge)[3:7]
  data_names <- colnames(data)
  merge_names <- c(data_names, clim_names)

  exceedances_clim <- base::merge(x = data_merge, y = exceedances_merge,
                                  all.y = TRUE, by = paste(substitute(x)))[,merge_names]

  if (returnDF) {
    exc_res <- list(threshold = as.data.frame(exceedances_clim),
                    exceedance = as.data.frame(exceedances))
  } else {
    exc_res <- list(threshold = data.table::setDT(exceedances_clim),
                    exceedance = data.table::setDT(exceedances))
  }
  return(exc_res)

}
