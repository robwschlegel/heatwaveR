#' Detect consecutive days in exceedance of a given threshold.
#'
#' @importFrom dplyr %>%
#'
#' @param data A data frame with at least the two following columns:
#' a \code{t} column which is a vector of dates of class \code{Date},
#' and a \code{temp} column, which is the temperature on those given
#' dates. If columns are named differently, their names can be supplied as \code{x}
#' and \code{y} (see below). The function will not accurately detect consecutive
#' days of temperatures in exceedance of the \code{threshold} if missing days of
#' data are not filled in with \code{NA}. Data of the appropriate format are created
#' by the function \code{\link{make_whole}}, but your own data may be used
#' directly if they meet the given criteria.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{make_whole}. If a column headed \code{t} is present in
#' the dataframe, this argument may be omitted; otherwise, specify the name of
#' the column with dates here.
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
#' integer. The default is \code{3} days.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily temperatures, with few missing values. The accompanying function
#' \code{\link{make_whole}} aids in the preparation of a time series that is
#' suitable for use with \code{exceedance}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{make_whole}}.
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
#' @return The function will return a list of two tibbles (see the \code{tidyverse}).
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
                       maxPadLength = FALSE) {

    # message("exceedance() is deprecated and will not be included in the next release of heatwaveR")
    # message("please use detecet_event() directly and set 'threshClim =' whatever your static threshold is")

    temp <- threshCriterion <- durationCriterion <- event <- event_no <- doy <- NULL

    ts_x <- eval(substitute(x), data)
    ts_y <- eval(substitute(y), data)
    ts_xy <- data.frame(ts_x = ts_x, ts_y = ts_y)
    rm(ts_x); rm(ts_y)

    ts_whole <- make_whole_fast(ts_xy)

    if (length(stats::na.omit(ts_whole$ts_y)) < length(ts_whole$ts_y) & is.numeric(maxPadLength)) {
      ts_whole <- na_interp(doy = ts_whole$doy,
                            x = ts_whole$ts_x,
                            y = ts_whole$ts_y,
                            maxPadLength = maxPadLength)
    }

    if (missing(threshold))
      stop("Oh no! Please provide a threshold against which to calculate exceedances.")
    # if (threshold > max(ts_whole$ts_y, na.rm = T)) {
    #   stop(paste("The given threshold value of ", threshold, " is greater than the maximum temperature of ",
    #              round(max(ts_whole$ts_y, na.rm = T), 2), " present in this time series.", sep = ""))
    # }
    # if (threshold < min(ts_whole$ts_y, na.rm = T)) {
    #   stop(paste("The given threshold value of ", threshold, " is less than the minimum temperature of ",
    #              round(min(ts_whole$ts_y, na.rm = T), 2), " present in this time series.", sep = ""))
    # }
    if (maxPadLength != FALSE & !is.numeric(maxPadLength))
      stop("Please ensure that 'maxPadLength' is either FALSE or a numeric/integer value.")

    if (below) {
      ts_whole$ts_y <- -ts_whole$ts_y
      threshold <- -threshold
    }

    ts_whole$thresh <- rep(threshold, nrow(ts_whole))
    ts_whole$threshCriterion <- ts_whole$ts_y > ts_whole$thresh

    # if (sum(stats::na.omit(ts_whole$threshCriterion)) < minDuration & below == FALSE) {
    #   stop(paste0("Not enough consecutive days above ", threshold, " to detect an event."))
    # }
    # if (sum(stats::na.omit(ts_whole$threshCriterion)) < minDuration & below == TRUE) {
    #   stop(paste0("Not enough consecutive days below ", abs(threshold), " to detect an event."))
    # }

    ts_whole$threshCriterion[is.na(ts_whole$threshCriterion)] <- FALSE

    exceedances_clim <- proto_event(ts_whole,
                                    criterion_column = ts_whole$threshCriterion,
                                    minDuration = minDuration,
                                    joinAcrossGaps = joinAcrossGaps,
                                    maxGap = maxGap) %>%
      dplyr::rename(exceedance = event,
                    exceedance_no = event_no) %>%
      dplyr::select(-doy)

    thresh <- intensity_mean <- intensity_max <- intensity_cumulative <-
      exceedance_rel_thresh <- intensity_mean_abs <- intensity_max_abs <-
      intensity_cum_abs <- ts_y <- exceedance_no <- row_index <- index_peak <-  NULL

    if (nrow(stats::na.omit(exceedances_clim)) > 0) {

      exceedances <- exceedances_clim %>%
        dplyr::mutate(row_index = seq_len(nrow(exceedances_clim)),
                      exceedance_rel_thresh = ts_y - thresh) %>%
        dplyr::filter(stats::complete.cases(exceedance_no)) %>%
        dplyr::group_by(exceedance_no) %>%
        dplyr::summarise(index_start = min(row_index),
                         index_peak = row_index[exceedance_rel_thresh == max(exceedance_rel_thresh)][1],
                         index_end = max(row_index),
                         duration = dplyr::n(),
                         date_start = min(ts_x),
                         date_peak = ts_x[exceedance_rel_thresh == max(exceedance_rel_thresh)][1],
                         date_end = max(ts_x),
                         intensity_mean = mean(exceedance_rel_thresh),
                         intensity_max = max(exceedance_rel_thresh),
                         intensity_var = sqrt(stats::var(exceedance_rel_thresh)),
                         intensity_cumulative = max(cumsum(exceedance_rel_thresh)),
                         intensity_mean_abs = mean(ts_y),
                         intensity_max_abs = max(ts_y),
                         intensity_var_abs = sqrt(stats::var(ts_y)),
                         intensity_cum_abs = max(cumsum(ts_y)), .groups = "drop")


      exceedance_rel_thresh <- ts_whole$ts_y - ts_whole$thresh
      A <- exceedance_rel_thresh[exceedances$index_start]
      B <- ts_whole$ts_y[exceedances$index_start - 1]
      C <- ts_whole$thresh[exceedances$index_start - 1]
      if (length(B) + 1 == length(A)) {
        B <- c(NA, B)
        C <- c(NA, C)
      }
      exceedance_rel_thresh_start <- 0.5 * (A + B - C)

      exceedances$rate_onset <- ifelse(
        exceedances$index_start > 1,
        (exceedances$intensity_max - exceedance_rel_thresh_start) / (as.numeric(
          difftime(exceedances$date_peak, exceedances$date_start, units = "days")) + 0.5),
        NA
      )

      D <- exceedance_rel_thresh[exceedances$index_end]
      E <- ts_whole$ts_y[exceedances$index_end + 1]
      G <- ts_whole$thresh[exceedances$index_end + 1]
      exceedance_rel_thresh_end <- 0.5 * (D + E - G)

      exceedances$rate_decline <- ifelse(
        exceedances$index_end < nrow(ts_whole),
        (exceedances$intensity_max - exceedance_rel_thresh_end) / (as.numeric(
          difftime(exceedances$date_end, exceedances$date_peak, units = "days")) + 0.5),
        NA
      )

      if (below) {
        exceedances <- exceedances %>% dplyr::mutate(
          intensity_mean = -intensity_mean,
          intensity_max = -intensity_max,
          intensity_cumulative = -intensity_cumulative,
          intensity_mean_abs = -intensity_mean_abs,
          intensity_max_abs = -intensity_max_abs,
          intensity_cum_abs = -intensity_cum_abs
        )
        exceedances_clim <- exceedances_clim %>% dplyr::mutate(
          ts_y = -ts_y,
          thresh = -thresh
        )
      }

    } else {
      exceedances <- data.frame(exceedance_no = NA, index_start = NA, index_peak = NA, index_end = NA,
                                duration = NA, date_start = NA, date_peak = NA, date_end = NA,
                                intensity_mean = NA, intensity_max = NA, intensity_var = NA,
                                intensity_cumulative = NA, intensity_mean_abs = NA,
                                intensity_max_abs = NA, intensity_var_abs = NA,
                                intensity_cum_abs = NA, rate_onset = NA, rate_decline = NA)
      exceedances <- tibble::as_tibble(exceedances)
    }

    exceedances <- dplyr::mutate_if(exceedances, is.numeric, round, 4)
    names(exceedances_clim)[1] <- paste(substitute(x))
    names(exceedances_clim)[2] <- paste(substitute(y))

    list(threshold = tibble::as_tibble(exceedances_clim),
         exceedance = tibble::as_tibble(exceedances))
  }
