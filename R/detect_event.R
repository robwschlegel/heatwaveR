#' Detect heatwaves and cold-spells.
#'
#' Applies the Hobday et al. (2016) marine heat wave definition to an input time
#' series of a given value (temperature) along with a daily date vector and
#' pre-calculated seasonal and threshold climatologies, which may either be
#' created with \code{\link{ts2clm}} or some other means.
#'
#' @importFrom dplyr n %>%
#'
#' @param data A data frame with at least four columns. In the default setting
#' (i.e. ommitting the arguments \code{x}, \code{y}, \code{seas}, and \code{thresh};
#' see immediately below), the data set is expected to have the headers \code{t},
#' \code{temp}, \code{seas_clim_year}, and \code{thresh_clim_year}. The \code{t}
#' column is a vector of dates of class \code{Date}, \code{temp} is the measured
#' variable (by default it is assumed to be temperature), \code{seas_clim_year} is
#' the seasonal cycle slimatology (366 days), and \code{thresh_clim_year} is the
#' threshold above which events may be detected. Data of the appropriate format are
#' created by the function \code{\link{ts2clm}}, but your own data can be supplied
#' if they meet the criteria specified by \code{\link{ts2clm}}. If the column names
#' of \code{data} match those outlined here, the following four arguments may be
#' ignored.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{ts2clm}. If a column headed \code{t} is present in
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param seas This function will assume that the seasonal climatology column
#' is called 'seas_clim_year' as this matches the output of \code{\link{ts2clm}}.
#' If the column name for the seasonal climatology is different, provide that here.
#' @param thresh The threshold climatology column should be called
#' 'thresh_clim_year'. If it is not, provide the name of the threshold column here.
#' @param minDuration Minimum duration for acceptance of detected events.
#' Default is \code{5} days.
#' @param joinAcrossGaps Boolean switch indicating whether to join MHWs which
#' occur before/after a short gap as specified by \code{maxGap}. Default
#' is \code{TRUE}.
#' @param maxGap Maximum length of gap allowed for the joining of MHWs. Default
#' is \code{2} days.
#' @param coldSpells Boolean specifying if the code should detect cold events
#' instead of heat events. The default is \code{FALSE}. Please note that the
#' climatological thresholds for cold-spells are calculated the same as for
#' heatwaves, meaning that \code{pctile} should be set the same regardless
#' if one is calculating heatwaves or cold-spells. For example, if one wants
#' to calculate heatwaves above the 90th percentile threshold
#' (the default) one sets \code{pctile = 90}. Likewise, if one would like to
#' identify the most intense cold-spells one must also set \code{pctile = 90},
#' even though cold spells are in fact simply the coldest extreme events in a
#' time series, which statistically equate to values below the 10th percentile.
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
#' interpolated over (through optional \code{max_pad_length}) will be set equal
#' to the seasonal climatology. This means they will trigger the end/start of
#' any adjacent temperature values which satisfy the event definition criteria.
#' \item If the code is used to detect cold events (\code{coldSpells} = TRUE),
#' then it works just as for heat waves except that events are detected as
#' deviations below the (100 - pctile)th percentile  (e.g., the 10th instead of
#' 90th) for at least 5 days. Intensities are reported as negative values and
#' represent the temperature anomaly below climatology.
#' \item If only the climatology for the time series is required, and not the
#' events themselves, this may be done by setting \code{clim_only} = TRUE.
#' \item If a 366 row dataframe is provided to \code{alt_clim_data} then the
#' \code{climatology_start} and \code{climatology_end} arguments may be
#' ommitted.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016). The marine cold spell option was
#' implemented in version 0.13 (21 Nov 2015) of the Python module as a result
#' of our preparation of Schlegel et al. (2017), wherein the cold events
#' receive a brief overview.
#'
#' @return The function will return a list of two tibbles (see the \code{tidyverse}),
#' \code{clim} and \code{event}, which are the climatology and events,
#' respectively. The climatology contains the full time series of daily temperatures,
#' as well as the the seasonal climatology, the threshold and various aspects of
#' the events that were detected. The software was designed for detecting extreme
#' thermal events, and the units specified below reflect that intended purpose.
#' However, the various other kinds of extreme events may be detected according
#' to the 'marine heat wave' specifications, and if that is the case, the appropriate
#' units need to be determined by the user.
#'   \item{doy}{Julian day (day-of-year). For non-leap years it runs 1...59 and
#'   61...366, while leap years run 1...366. This column will be named differently if
#'   another name was specified to the \code{doy} argument.}
#'   \item{t}{The date of the temperature measurement. This column will be
#'   named differently if another name was specified to the \code{x} argument.}
#'   \item{temp}{If the software was used for the purpose for which it was designed,
#'   seawater temperature [deg. C] on the specified date will be returned. This
#'   column will of course be named differently if another kind of measurement was
#'   specified to the \code{y} argument.}
#'   \item{seas_clim_year}{Climatological seasonal cycle [deg. C].}
#'   \item{thresh_clim_year}{Seasonally varying threshold (e.g., 90th
#'   percentile) [deg. C].}
#'   \item{var_clim_year}{Seasonally varying variance (standard deviation) [deg. C].}
#'   \item{thresh_criterion}{Boolean indicating if \code{temp} exceeds
#'   \code{thresh_clim_year}.}
#'   \item{duration_criterion}{Boolean indicating whether periods of consecutive
#'   \code{thresh_criterion} are >= \code{min_duration}.}
#'   \item{event}{Boolean indicating if all criteria that define a MHW or MCS are
#'   met.}
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   occurence of the MHWs or MCSs.}
#'
#' The events are summarised using a range of event metrics:
#'   \item{event_no}{A sequential number indicating the ID and order of
#'   the events.}
#'   \item{index_start}{Start index of event.}
#'   \item{index_stop}{Stop index of event.}
#'   \item{duration}{Duration of event [days].}
#'   \item{date_start}{Start date of event [date].}
#'   \item{date_stop}{Stop date of event [date].}
#'   \item{date_peak}{Date of event peak [date].}
#'   \item{int_mean}{Mean intensity [deg. C].}
#'   \item{int_max}{Maximum (peak) intensity [deg. C].}
#'   \item{int_var}{Intensity variability (standard deviation) [deg. C].}
#'   \item{int_cum}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of event [deg. C / day].}
#'   \item{rate_decline}{Decline rate of event [deg. C / day].}
#'
#' \code{int_max_rel_thresh}, \code{int_mean_rel_thresh},
#' \code{int_var_rel_thresh}, and \code{int_cum_rel_thresh}
#' are as above except relative to the threshold (e.g., 90th percentile) rather
#' than the seasonal climatology.
#'
#' \code{int_max_abs}, \code{int_mean_abs}, \code{int_var_abs}, and
#' \code{int_cum_abs} are as above except as absolute magnitudes
#' rather than relative to the seasonal climatology or threshold.
#'
#' \code{int_max_norm} and \code{int_mean_norm} are as above except
#' units are in multiples of threshold exceedances, i.e., a value of 1.5
#' indicates the event intensity (relative to the climatology) was 1.5 times the
#' value of the threshold (relative to climatology,
#' i.e., threshold - climatology.)
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
#' res_clim <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res_event <- detect_event(res_clim)
#' # show a portion of the climatology:
#' res_event$clim[1:10, ]
#' # show some of the heat waves:
#' res_event$event[1:5, 1:10]
detect_event <- function(data,
                         x = t,
                         y = temp,
                         seas = seas_clim_year,
                         thresh = thresh_clim_year,
                         minDuration = 5,
                         joinAcrossGaps = TRUE,
                         maxGap = 2,
                         coldSpells = FALSE) {

  if(!(is.numeric(minDuration)))
    stop("Please ensure that 'minDuration' is a numeric/integer value.")
  if(!(is.logical(joinAcrossGaps)))
    stop("Please ensure that 'joinAcrossGaps' is either TRUE or FALSE.")
  if(!(is.numeric(maxGap)))
    stop("Please ensure that 'maxGap' is a numeric/integer value.")

  temp <- seas_clim_year <- thresh_clim_year <- NULL

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)
  ts_seas <- eval(substitute(seas), data)
  ts_thresh <- eval(substitute(thresh), data)
  t_series <- tibble::tibble(ts_x, ts_y, ts_seas, ts_thresh)
  # rm(ts_x); rm(ts_y); rm(ts_seas); rm(ts_thresh)

  if (coldSpells){
    t_series$ts_y <- -t_series$ts_y
    t_series$ts_seas <- -t_series$ts_seas
    t_series$ts_thresh <- -t_series$ts_thresh
  }

  t_series$ts_y[is.na(t_series$ts_y)] <- t_series$ts_seas[is.na(t_series$ts_y)]
  t_series$thresh_criterion <- t_series$ts_y > t_series$ts_thresh

  proto_1 <- proto_event(t_series, criterion_column = 5, minDuration)
  t_series$duration_criterion <- rep(FALSE, nrow(t_series))
  for (i in 1:nrow(proto_1)) {
    t_series$duration_criterion[proto_1$index_start[i]:proto_1$index_stop[i]] <-
      rep(TRUE, length = proto_1$duration[i])
  }

  proto_2 <- proto_event(t_series, criterion_column = 6,
                                     minDuration, gaps = TRUE)

  if (ncol(proto_2) == 4)
    joinAcrossGaps <- FALSE

  if (joinAcrossGaps) {
    t_series$event <- t_series$duration_criterion
    for (i in 1:nrow(proto_2)) {
      t_series$event[proto_2$index_start[i]:proto_2$index_stop[i]] <-
        rep(TRUE, length = proto_2$duration[i])
    }
  } else {
    t_series$event <- t_series$duration_criterion
  }

  proto_3 <- proto_event(t_series, criterion_column = 7, minDuration)

  t_series$event_no <- rep(NA, nrow(t_series))
  for (i in 1:nrow(proto_3)) {
    t_series$event_no[proto_3$index_start[i]:proto_3$index_stop[i]] <-
      rep(i, length = proto_3$duration[i])
  }

  int_mean <- int_max <- int_cum <- int_mean_rel_thresh <-
    int_max_rel_thresh <- int_cum_rel_thresh <- int_mean_abs <-
    int_max_abs <- int_cum_abs <- int_mean_norm <- int_max_norm <-
    rate_onset <- rate_decline <- mhw_rel_thresh <-
    rel_thresh_norm <- mhw_rel_seas <- event_no <- row_index <-  NULL

  events <- t_series %>%
    dplyr::mutate(row_index = 1:nrow(t_series),
                  mhw_rel_seas = ts_y - ts_seas,
                  mhw_rel_thresh = ts_y - ts_thresh,
                  # The missing brackets around this equation may be an error...
                  # But it matches the output of the original
                  rel_thresh_norm = ts_y - ts_thresh / ts_thresh - ts_seas) %>%
    dplyr::filter(stats::complete.cases(event_no)) %>%
    dplyr::group_by(event_no) %>%
    dplyr::summarise(index_start = min(row_index),
                     index_stop = max(row_index),
                     duration = n(),
                     date_start = min(ts_x),
                     date_stop = max(ts_x),
                     date_peak = ts_x[mhw_rel_seas == max(mhw_rel_seas)][1],
                     int_mean = mean(mhw_rel_seas),
                     int_max = max(mhw_rel_seas),
                     int_var = sqrt(stats::var(mhw_rel_seas)),
                     int_cum = max(cumsum(mhw_rel_seas)),
                     int_mean_rel_thresh = mean(mhw_rel_thresh),
                     int_max_rel_thresh = max(mhw_rel_thresh),
                     int_var_rel_thresh = sqrt(stats::var(mhw_rel_thresh)),
                     int_cum_rel_thresh = max(cumsum(mhw_rel_thresh)),
                     int_mean_abs = mean(ts_y),
                     int_max_abs = max(ts_y),
                     int_var_abs = sqrt(stats::var(ts_y)),
                     int_cum_abs = max(cumsum(ts_y)),
                     int_mean_norm = mean(rel_thresh_norm),
                     int_max_norm = max(rel_thresh_norm))
  # Note that this creates an output with a different column order than the original
  # 'event_no' is now the first column, rather than the third

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
    (events$int_max - mhw_rel_seas_start) / (as.numeric(
      difftime(events$date_peak, events$date_start, units = "days")) + 0.5),
    NA
  )

  D <- mhw_rel_seas[events$index_stop]
  E <- t_series$ts_y[events$index_stop + 1]
  F <- t_series$ts_seas[events$index_stop + 1]
  mhw_rel_seas_end <- 0.5 * (D + E - F)

  events$rate_decline <- ifelse(
    events$index_stop < nrow(t_series),
    (events$int_max - mhw_rel_seas_end) / (as.numeric(
      difftime(events$date_stop, events$date_peak, units = "days")) + 0.5),
    NA
  )

  if (coldSpells) {
    events <- events %>% dplyr::mutate(
      int_mean = -int_mean,
      int_max = -int_max,
      int_cum = -int_cum,
      int_mean_rel_thresh = -int_mean_rel_thresh,
      int_max_rel_thresh = -int_max_rel_thresh,
      int_cum_rel_thresh = -int_cum_rel_thresh,
      int_mean_abs = -int_mean_abs,
      int_max_abs = -int_max_abs,
      int_cum_abs = -int_cum_abs,
      int_mean_norm = -int_mean_norm,
      int_max_norm = -int_max_norm,
      rate_onset = -rate_onset,
      rate_decline = -rate_decline
    )
    # This should no longer be necessary...
    # t_series <- t_series %>% dplyr::mutate(
    #   ts_y = -ts_y,
    #   ts_seas = -ts_seas,
    #   ts_thresh = -ts_thresh
    # )
  }

  data_clim <- cbind(data, t_series[,5:8])

  list(clim = tibble::as_tibble(data_clim),
       event = tibble::as_tibble(events))
}

