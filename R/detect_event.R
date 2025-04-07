#' Detect heatwaves and cold-spells.
#'
#' Applies the Hobday et al. (2016) marine heat wave definition to an input time
#' series of a given value (usually, but not necessarily limited to, temperature)
#' along with a daily date vector and pre-calculated seasonal and threshold
#' climatologies, which may either be created with \code{\link{ts2clm}} or some
#' other means.
#'
#' @param data A data frame with at least four columns. In the default setting
#' (i.e. omitting the arguments \code{x}, \code{y}, \code{seas}, and \code{thresh};
#' see immediately below), the data set is expected to have the headers \code{t},
#' \code{temp}, \code{seas}, and \code{thresh}. The \code{t}
#' column is a vector of dates of class \code{Date}, \code{temp} is the measured
#' variable (by default it is assumed to be temperature), \code{seas} is the seasonal
#' cycle daily climatology (366 days), and \code{thresh} is the seasonal cycle
#' daily threshold above which events may be detected. Data of the appropriate
#' format are created by the function \code{\link{ts2clm}}, but your own data
#' can be supplied if they meet the criteria specified by \code{\link{ts2clm}}.
#' If the column names of \code{data} match those outlined here, the following
#' four arguments may be ignored. Note that it is also possible to provide hourly
#' data in the \code{x} column as class \code{POSIXct}.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{\link{ts2clm}}. If a column headed \code{t} is present in
#' the dataframe, this argument may be omitted; otherwise, specify the name of
#' the column with dates here. Note that it is also possible to provide hourly
#' data as class \code{POSIXct}.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param seasClim The dafault for this argument assumes that the seasonal
#' climatology column is called \code{seas} as this matches the output of
#' \code{\link{ts2clm}}. If the column name for the seasonal climatology is
#' different, provide that here.
#' @param threshClim The threshold climatology column should be called
#' \code{thresh}. If it is not, provide the name of the threshold column here.
#' @param threshClim2 If one wishes to provide a second climatology threshold
#' filter for the more rigorous detection of events, a vector or column containing
#' logical values (i.e. TRUE FALSE) should be provided here. By default this
#' argument is ignored. It's primary purpose is to allow for the inclusion of
#' tMin and tMax thresholds.
#' @param minDuration The minimum duration for acceptance of detected events.
#' The default is \code{5} days.
#' @param minDuration2 The minimum duration for acceptance of events after
#' filtering by \code{threshClim} and \code{threshClim}. By default
#' \code{minDuration2 = minDuration} and is ignored if \code{threshClim2} has not
#' been specified.
#' @param joinAcrossGaps Boolean switch indicating whether to join events which
#' occur before/after a short gap as specified by \code{maxGap}. The default
#' is \code{TRUE}.
#' @param maxGap The maximum length of gap allowed for the joining of MHWs. The
#' default is \code{2} time steps.
#' @param maxGap2 The maximum gap length after applying both thresholds.
#' By default \code{maxGap2 = maxGap} and is ignored if \code{threshClim2} has not
#' been specified.
#' @param coldSpells Boolean specifying if the code should detect cold events
#' instead of warm events. The default is \code{FALSE}. Please note that the
#' climatological thresholds for cold-spells are considered to be the inverse
#' of those for MHWs. For example, the default setting for the detection of
#' MHWs is \code{pctile = 90}, as seen in \code{\link{ts2clm}}. Should one want
#' to use \code{detect_event} for MCSs, this threshold would best be generated
#' in \code{\link{ts2clm}} by setting \code{pctile = 10} (see example below).
#' Any value may be used, but this is the setting used for the calculation of
#' MCSs in Schlegel et al. (2017a).
#' @param protoEvents The default, \code{protoEvents = FALSE}, will return the full
#' output comprised of a list of two data frames, one with the \code{climatology}
#' and the other with the \code{event} metrics. See \strong{Value} below.
#' If \code{protoEvents = TRUE}, the output will contain the original time series
#' together with columns indicating if the threshold criterion
#' (\code{threshCriterion}) and duration criterion (\code{durationCriterion})
#' have been exceeded, a column showing if a heatwave is present
#' (i.e. both \code{threshCriterion} and \code{durationCriterion}
#' \code{TRUE}), and a sequential number uniquely identifying the detected event(s);
#' heatwave metrics will not be reported in the \code{event} dataframe. Note also that
#' if \code{protoEvents = TRUE} it will ignore whatever the user provides to the
#' \code{categories} argument and anything else passed to \code{...}.
#' @param categories Rather than using \code{\link{category}} as a separate step to determine
#' the categories of the detected MHWs, one may choose to set this argument to \code{TRUE}.
#' One may pass the same arguments used in the \code{\link{category}} function to this function
#' to affect the output. Note that the default behaviour of \code{\link{category}} is to
#' return the event data only. To return the same list structure that \code{\link{detect_event}}
#' outputs by default, add the argument \code{climatology = TRUE}.
#' @param roundRes This argument allows the user to choose how many decimal places
#' the MHW metric outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundRes = FALSE}. This argument may only be given
#' numeric values or FALSE.
#' @param returnDF The default (\code{TRUE}) tells the function to return the results as
#' type \code{data.frame}. \code{FALSE} will return the results as a \code{data.table}.
#' @param ... Other arguments that will be passed internally to \code{\link{category}}
#' when \code{categories = TRUE}. See the documentation for \code{\link{category}} for the
#' list of possible arguments.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily values with few missing values. Time ranges which start and end
#' part-way through the calendar year are supported. The accompanying function
#' \code{\link{ts2clm}} aids in the preparation of a time series that is
#' suitable for use with \code{detect_event}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{ts2clm}}.
#' \item The calculation of onset and decline rates assumes that the events
#' started a half-day before the start day and ended a half-day after the
#' end-day. This is consistent with the duration definition as implemented,
#' which assumes duration = end day - start day + 1. An event that is already
#' present at the beginning of a time series, or an event that is still present
#' at the end of a time series, will report the rate of onset or the rate of
#' decline as \code{NA}, as it is impossible to know what the temperature half a
#' day before or after the start or end of the event is.
#' \item For the purposes of event detection, any missing temperature values not
#' interpolated over (through optional \code{maxPadLength} in \code{\link{ts2clm}})
#' will be set equal to the seasonal climatology. This means they will trigger
#' the end/start of any adjacent temperature values which satisfy the event
#' definition criteria.
#' \item If the code is used to detect cold events (\code{coldSpells = TRUE}),
#' then it works just as for heat waves except that events are detected as
#' deviations below the (100 - pctile)th percentile (e.g., the 10th instead of
#' 90th) for at least 5 days. Intensities are reported as negative values and
#' represent the temperature anomaly below climatology.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016). The marine cold spell option was
#' implemented in version 0.13 (21 Nov 2015) of the Python module as a result
#' of our preparation of Schlegel et al. (2017), wherein the cold events
#' receive a brief overview.
#'
#' @return The function will return a list of two data.frames,
#' \code{climatology} and \code{event}, which are, surprisingly, the climatology
#' and event results, respectively. The climatology contains the full time series of
#' daily temperatures, as well as the the seasonal climatology, the threshold
#' and various aspects of the events that were detected. The software was
#' designed for detecting extreme thermal events, and the units specified below
#' reflect that intended purpose. However, various other kinds of extreme
#' events may be detected according to the specifications, and if that is the
#' case, the appropriate units need to be determined by the user.
#'
#' The \code{climatology} results will contain the same column produced by
#' \code{\link{ts2clm}} as well as the following:
#'   \item{threshCriterion}{Boolean indicating if \code{temp} exceeds
#'   \code{thresh}.}
#'   \item{durationCriterion}{Boolean indicating whether periods of consecutive
#'   \code{threshCriterion} are >= \code{min_duration}.}
#'   \item{event}{Boolean indicating if all criteria that define an extreme event
#'   are met.}
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   occurrence of the events.}
#'   \item{intensity}{The difference between \code{temp} (or whichever column is provided
#'   for \code{y}) and \code{seas}. Only added if \code{categories = TRUE}
#'   and \code{climatology = TRUE}.}
#'   \item{category}{The category classification per day. Only added
#'   if \code{categories = TRUE} and \code{climatology = TRUE}.}
#'
#' The \code{event} results are summarised using a range of event metrics:
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   the events.}
#'   \item{index_start}{Start index of event.}
#'   \item{index_end}{End index of event.}
#'   \item{duration}{Duration of event [days].}
#'   \item{date_start}{Start date of event [date].}
#'   \item{date_end}{End date of event [date].}
#'   \item{date_peak}{Date of event peak [date].}
#'   \item{intensity_mean}{Mean intensity [deg. C].}
#'   \item{intensity_max}{Maximum (peak) intensity [deg. C].}
#'   \item{intensity_var}{Intensity variability (standard deviation) [deg. C].}
#'   \item{intensity_cumulative}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of event [deg. C / day].}
#'   \item{rate_decline}{Decline rate of event [deg. C / day].}
#'   \item{event_name}{The name of the event. Generated from the \code{name}
#'   value provided and the year of the \code{date_peak} of
#'   the event. If no \code{name} value is provided the default "Event" is used.
#'   As proposed in Hobday et al. (2018), \code{Moderate} events are not given a name
#'   so as to prevent multiple repeat names within the same year. If two or more events
#'   ranked greater than Moderate are reported within the same year, they will be
#'   differentiated with the addition of a trailing letter
#'   (e.g. Event 2001a, Event 2001b). Only added if \code{categories = TRUE}.}
#'   \item{category}{The maximum category threshold reached/exceeded by the event.
#'   Only added if \code{categories = TRUE}.}
#'   \item{p_moderate}{The proportion of the total duration (days) spent at or above
#'   the first threshold, but below any further thresholds. Only added if \code{categories = TRUE}.}
#'   \item{p_strong}{The proportion of the total duration (days) spent at or above
#'   the second threshold, but below any further thresholds. Only added if \code{categories = TRUE}.}
#'   \item{p_severe}{The proportion of the total duration (days) spent at or above
#'   the third threshold, but below the fourth threshold. Only added if \code{categories = TRUE}.}
#'   \item{p_extreme}{The proportion of the total duration (days) spent at or above
#'   the fourth and final threshold. Only added if \code{categories = TRUE}.}
#'   \item{season}{The season(s) during which the event occurred. If the event
#'   occurred across two seasons this will be displayed as e.g. "Winter/Spring".
#'   Across three seasons as e.g. "Winter-Summer". Events lasting across four or more
#'   seasons are listed as "Year-round". December (June) is used here as the start of
#'   Austral (Boreal) summer. If "start", "peak", or "end" was given to the \code{season}
#'   argument then only the one season during that chosen period will be given.
#'   Only added if \code{categories = TRUE}.}
#'
#' \code{intensity_max_relThresh}, \code{intensity_mean_relThresh},
#' \code{intensity_var_relThresh}, and \code{intensity_cumulative_relThresh}
#' are as above except relative to the threshold (e.g., 90th percentile) rather
#' than the seasonal climatology.
#'
#' \code{intensity_max_abs}, \code{intensity_mean_abs}, \code{intensity_var_abs}, and
#' \code{intensity_cumulative_abs} are as above except as absolute magnitudes
#' rather than relative to the seasonal climatology or threshold.
#'
#' Note that \code{rate_onset} and \code{rate_decline} will return \code{NA}
#' when the event begins/ends on the first/last day of the time series. This
#' may be particularly evident when the function is applied to large gridded
#' data sets. Although the other metrics do not contain any errors and
#' provide sensible values, please take this into account in its
#' interpretation.
#'
#' @author Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016). A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi:10.1016/j.pocean.2015.12.014
#'
#' Schlegel, R. W., Oliver, C. J., Wernberg, T. W., Smit, A. J. (2017).
#' Nearshore and offshore co-occurrences of marine heatwaves and cold-spells.
#' Progress in Oceanography, 151, pp. 189-205, doi:10.1016/j.pocean.2017.01.004
#'
#' @export
#'
#' @examples
#' data.table::setDTthreads(threads = 1)
#' res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' out <- detect_event(res_clim)
#' # show a portion of the climatology:
#' out$climatology[1:10, ]
#' # show some of the heat waves:
#' out$event[1:5, 1:10]
#'
#' # Or if one wants to calculate MCSs
#' res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"),
#'                    pctile = 10)
#' out <- detect_event(res_clim, coldSpells = TRUE)
#' # show a portion of the climatology:
#' out$climatology[1:10, ]
#' # show some of the cold-spells:
#' out$event[1:5, 1:10]
#'
#' # It is also possible to calculate the categories of events directly
#' # See the \code{\link{category}} documentation for more functionality
#' res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' out_event <- detect_event(res_clim, categories = TRUE)
#' out_list <- detect_event(res_clim, categories = TRUE, climatology = TRUE)
#'
#' # It is also possible to give two separate sets of threshold criteria
#'
#' # To use a second static threshold we first use the exceedance function
#' thresh_19 <- exceedance(sst_Med, threshold = 19, minDuration = 10, maxGap = 0)$threshold
#' # Then we use that output when detecting our events
#' events_19 <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")),
#'                          threshClim2 = thresh_19$exceedance, minDuration2 = 10, maxGap2 = 0)
#'
#' # If we want to use two different percentile thresholds we use detect_event
#' thresh_95 <- detect_event(ts2clm(sst_Med, pctile = 95,
#'                                  climatologyPeriod = c("1982-01-01", "2011-12-31")),
#'                           minDuration = 2, maxGap = 0)$climatology
#' # Then we use that output when detecting our events
#' events_95 <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31")),
#'                           threshClim2 = thresh_95$event, minDuration2 = 2, maxGap2 = 0)
#'
detect_event <- function(data,
                         x = t,
                         y = temp,
                         seasClim = seas,
                         threshClim = thresh,
                         threshClim2 = NA,
                         minDuration = 5,
                         minDuration2 = minDuration,
                         joinAcrossGaps = TRUE,
                         maxGap = 2,
                         maxGap2 = maxGap,
                         coldSpells = FALSE,
                         protoEvents = FALSE,
                         categories = FALSE,
                         roundRes = 4,
                         returnDF = TRUE,
                         ...) {

  if (!(is.numeric(minDuration)))
    stop("Please ensure that 'minDuration' is a numeric/integer value.")
  if (!(is.logical(joinAcrossGaps)))
    stop("Please ensure that 'joinAcrossGaps' is either TRUE or FALSE.")
  if (!(is.numeric(maxGap)))
    stop("Please ensure that 'maxGap' is a numeric/integer value.")
  if (!(is.numeric(roundRes))) {
    if (!roundRes == FALSE) {
      stop("Please ensure that 'roundRes' is either a numeric value or FALSE.")
    }
  }

  # Ensure ts2clm returns a data.table
  # Do a check: if already a data.table, carry on; setDT if a data.frame
  # Ensure all code from here on is translated to data.table, incl. proto_events
  # testing...
  # data <- res_ts

  temp <- seas <- thresh <- threshCriterion <- durationCriterion <- event <- NULL

  ts_x <- eval(substitute(x), data)
  if (is.null(ts_x) | is.function(ts_x))
    stop("Please ensure that a column named 't' is present in your data.frame or that you have assigned a column to the 'x' argument.")
  ts_y <- eval(substitute(y), data)
  if (is.null(ts_y) | is.function(ts_y))
    stop("Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
  ts_seas <- eval(substitute(seasClim), data)
  if (is.null(ts_seas) | is.function(ts_seas))
    stop("Please ensure that a column named 'seas' is present in your data.frame or that you have assigned a column to the 'seasClim' argument.")
  ts_thresh <- eval(substitute(threshClim), data)
  if (is.null(ts_thresh) | is.function(ts_thresh))
    stop("Please ensure that a column named 'thresh' is present in your data.frame or that you have assigned a column to the 'threshClim' argument.")
  # t_series <- data.table::data.table(ts_x = data$t, ts_y = data$temp, ts_seas = data$seas, ts_thresh = data$thresh)
  t_series <- data.table::data.table(ts_x,
                                     ts_y,
                                     ts_seas,
                                     ts_thresh)
  rm(ts_x, ts_y, ts_seas, ts_thresh)

  if (coldSpells) {

    t_series$ts_y <- -t_series$ts_y
    t_series$ts_seas <- -t_series$ts_seas
    t_series$ts_thresh <- -t_series$ts_thresh
    # NB: This only internally attributes negative values
    # Meaning the following threshCriterion code does not work as expected
    # t_series[, ts_y := -ts_y]
    # t_series[, ts_seas := -ts_seas]
    # t_series[, ts_thresh := -ts_thresh]

  }

  t_series[is.na(ts_y), ts_y := ts_seas]
  t_series[, threshCriterion := !is.na(ts_y) & ts_y > ts_thresh]

  # Below: make sure proto_events returns a data.table

  events_clim <- proto_event(t_series,
                             criterion_column = t_series$threshCriterion,
                             minDuration = minDuration,
                             joinAcrossGaps = joinAcrossGaps,
                             maxGap = maxGap)

  if (!is.na(threshClim2[1])) {
    if (!is.logical(threshClim2[1]))
      stop("Please ensure 'threshClim2' contains logical values (e.g. TRUE and/or FALSE)")
    events_clim <- proto_event(t_series,
                               criterion_column = events_clim$event & threshClim2,
                               minDuration = minDuration2,
                               joinAcrossGaps = joinAcrossGaps,
                               maxGap = maxGap2)
  }

  if (protoEvents) {

    events_clim <- data.table::data.table(data,
                                          threshCriterion = events_clim$threshCriterion,
                                          durationCriterion = events_clim$durationCriterion,
                                          event = events_clim$event,
                                          event_no = events_clim$event_no)

    if (returnDF) data.table::setDF(events_clim)
    return(events_clim)

  } else {

    intensity_mean <- intensity_max <- intensity_cumulative <- intensity_mean_relThresh <-
      intensity_max_relThresh <- intensity_cumulative_relThresh <- intensity_mean_abs <-
      intensity_max_abs <- intensity_cumulative_abs <- rate_onset <- rate_decline <-
      mhw_rel_thresh <- mhw_rel_seas <- event_no <- row_index <- index_start <- index_peak <-
      index_end <- NULL

    if (nrow(stats::na.omit(events_clim)) > 0) {
      events <- data.table(events_clim,
                           row_index = base::seq_len(nrow(events_clim)),
                           mhw_rel_seas = events_clim$ts_y - events_clim$ts_seas,
                           mhw_rel_thresh = events_clim$ts_y - events_clim$ts_thresh)

      events <- events[!is.na(event_no)]

      year <- ts_x <- doy <- NULL

      events <- events[, list(
        index_start = min(row_index),
        index_peak = row_index[which.max(mhw_rel_seas)][1],
        index_end = max(row_index),
        duration = max(row_index) - min(row_index) + 1,
        date_start = min(ts_x),
        date_peak = ts_x[which.max(mhw_rel_seas)][1],
        date_end = max(ts_x),
        intensity_mean = mean(mhw_rel_seas),
        intensity_max = max(mhw_rel_seas),
        intensity_var = stats::sd(mhw_rel_seas),
        intensity_cumulative = sum(mhw_rel_seas),
        intensity_mean_relThresh = mean(mhw_rel_thresh),
        intensity_max_relThresh = max(mhw_rel_thresh),
        intensity_var_relThresh = stats::sd(mhw_rel_thresh),
        intensity_cumulative_relThresh = sum(mhw_rel_thresh),
        intensity_mean_abs = mean(ts_y),
        intensity_max_abs = max(ts_y),
        intensity_var_abs = stats::sd(ts_y),
        intensity_cumulative_abs = sum(ts_y)
      ), by = list(event_no)]

      mhw_rel_seas <- t_series$ts_y - t_series$ts_seas
      A <- mhw_rel_seas[events$index_start]
      B <- t_series$ts_y[events$index_start - 1]
      C <- t_series$ts_seas[events$index_start - 1]
      if (length(B) + 1 == length(A)) {
        B <- c(NA, B)
        C <- c(NA, C)
      }
      mhw_rel_seas_start <- 0.5 * (A + B - C)

      events$rate_onset <- ifelse(
        events$index_start > 1,
        (events$intensity_max - mhw_rel_seas_start) / (as.numeric(
          difftime(events$date_peak, events$date_start, units = "days")) + 0.5),
        NA
      )

      D <- mhw_rel_seas[events$index_end]
      E <- t_series$ts_y[events$index_end + 1]
      G <- t_series$ts_seas[events$index_end + 1]
      mhw_rel_seas_end <- 0.5 * (D + E - G)

      events$rate_decline <- ifelse(
        events$index_end < nrow(t_series),
        (events$intensity_max - mhw_rel_seas_end) / (as.numeric(
          difftime(events$date_end, events$date_peak, units = "days")) + 0.5),
        NA
      )

      if (coldSpells) {
        events$intensity_mean <- -events$intensity_mean
        events$intensity_max <- -events$intensity_max
        events$intensity_cumulative <- -events$intensity_cumulative
        events$intensity_mean_relThresh <- -events$intensity_mean_relThresh
        events$intensity_max_relThresh <- -events$intensity_max_relThresh
        events$intensity_cumulative_relThresh <- -events$intensity_cumulative_relThresh
        events$intensity_mean_abs <- -events$intensity_mean_abs
        events$intensity_max_abs <- -events$intensity_max_abs
        events$intensity_cumulative_abs <- -events$intensity_cumulative_abs
        events$rate_onset <- -events$rate_onset
        events$rate_decline <- -events$rate_decline
      }

    } else {
      events <- data.frame(event_no = NA, index_start = NA, index_peak = NA, index_end = NA,
                           duration = NA, date_start = NA, date_peak = NA, date_end = NA,
                           intensity_mean = NA, intensity_max = NA, intensity_var = NA,
                           intensity_cumulative = NA, intensity_mean_relThresh = NA,
                           intensity_max_relThresh = NA, intensity_var_relThresh = NA,
                           intensity_cumulative_relThresh = NA, intensity_mean_abs = NA,
                           intensity_max_abs = NA, intensity_var_abs = NA,
                           intensity_cumulative_abs = NA, rate_onset = NA, rate_decline = NA)
    }

    event_cols <- names(events)[9:22]
    clim_cols <- names(events_clim)[2:4]
    if (nrow(events) == 1) {
      if (is.na(events$rate_onset)) {
        event_cols <- event_cols[-grep(pattern = "rate_onset", x = event_cols, value = FALSE)]
      }
      if (is.na(events$rate_decline)) {
        event_cols <- event_cols[-grep(pattern = "rate_decline", x = event_cols, value = FALSE)]
      }
    }

    if (is.numeric(roundRes)) {
      if (nrow(events) > 0) {
        events <- as.data.frame(events)
        events_clim <- as.data.frame(events_clim)
        events[,event_cols] <- round(events[,event_cols], roundRes)
        events_clim[,clim_cols] <- round(events_clim[,clim_cols], roundRes)
      }
    }

    data_clim <- cbind(data, events_clim[,5:8])

    data_res <- list(climatology = data_clim, event = events)

    if (categories) {
      data_temp <- list(climatology = events_clim, event = events)
      colnames(data_temp$climatology)[1:4] <- c("t", "temp", "seas", "thresh")

      if (coldSpells) {
        data_temp$climatology$temp <- -data_temp$climatology$temp
        data_temp$climatology$seas <- -data_temp$climatology$seas
        data_temp$climatology$thresh <- -data_temp$climatology$thresh
      }

      if ("lat" %in% colnames(data)) {
        data_temp$climatology$lat <- data$lat
      }
      if ("latitude" %in% colnames(data)) {
        data_temp$climatology$lat <- data$latitude
      }

      data_cat <- category(data_temp, ...)

      if (is.data.frame(data_cat)) {
        colnames(data_cat)[c(3,5)] <- c("date_peak", "intensity_max")
        data_res <- base::merge(x = events, y = data_cat,
                                by = c("event_no", "duration", "intensity_max", "date_peak"))
        data_res <- data_res[order(data_res$event_no),]
        data_res <- data_res[,c(1,5,6,7,2,8,4,9,10,3,11:29)]
        row.names(data_res) <- NULL

      } else {
        colnames(data_cat$event)[c(3,5)] <- c("date_peak", "intensity_max")
        data_res <- list(climatology = base::merge(x = data_res$climatology, y = data_cat$climatology,
                                                   by = c("t", "event_no"), all.x = TRUE),
                         event = base::merge(x = data_res$event, y = data_cat$event,
                                             by = c("event_no", "duration", "intensity_max", "date_peak")))

        type_cols <- base::sapply(data_res$climatology, class)
        date_cols <- colnames(data_res$climatology)[which(type_cols == "Date")]
        data_cols <- colnames(data)[!colnames(data) %in% colnames(data_cat$climatology)]
        other_cols <- colnames(data_res$climatology)[!colnames(data_res$climatology) %in% c(date_cols, data_cols)]

        data_res$climatology <- data_res$climatology[, c(date_cols, data_cols, other_cols)]
        data_res$climatology <- data_res$climatology[base::order(data_res$climatology$t),]
        data_res$event <- data_res$event[order(data_res$event$event_no),]
        data_res$event <- data_res$event[,c(1,5,6,7,2,8,4,9,10,3,11:29)]
        row.names(data_res$event) <- NULL
      }
    }

    if (returnDF) {
      if(base::class(data_res) == "list") {
        data.table::setDF(data_res$climatology)
        data.table::setDF(data_res$event)
      } else {
        data.table::setDF(data_res)
      }
    } else {
      if(base::class(data_res) == "list") {
        data.table::setDT(data_res$climatology)
        data.table::setDT(data_res$event)
      } else {
        data.table::setDT(data_res)
      }
    }
    return(data_res)

  }
}
