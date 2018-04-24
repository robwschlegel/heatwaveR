#' Detect consecutive days in exceedance of a given threshold.
#'
#' @importFrom tidyr %>%
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
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param threshold The static threshold used to determine how many consecutive
#' days are in exceedance of the temperature of interest. Default is
#' \code{20} degrees.
#' @param below Default is \code{FALSE}. When set to TRUE, consecutive days of temperature
#' below the \code{threshold} variable are calculated. When set to FALSE,
#' consecutive days above the \code{threshold} variable are calculated.
#' @param min_duration Minimum duration that temperatures must be in exceedance
#' of the \code{threshold} variable. Default is \code{5} days.
#' @param join_across_gaps A TRUE/FALSE statement that indicates whether
#' or not to join consecutive days of temperatures in exceedance of the
#' \code{threshold} across a small gap between groups before/after a short
#' gap as specified by \code{max_gap}. Default is \code{TRUE}.
#' @param max_gap The maximum length of the gap across which to connect
#' consecutive days in exceedance of the \code{threshold} when
#' \code{join_across_gaps} is \code{TRUE}.
#' @param max_pad_length Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{max_pad_length} will be left as \code{NA}. Set as an
#' integer. Default is \code{3} days.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily temperatures, with few missing values. The accompanying function
#' \code{\link{make_whole}} aids in the preparation of a time series that is
#' suitable for use with \code{exceedance}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{make_whole}}.
#' \item Future versions seek to accomodate monthly and annual time series, too.
#' \item The calculation of onset and decline rates assumes that exceedance of the
#' \code{threshold} started a half-day before the start day and ended a half-day
#' after the end-day. This is consistent with the duration definition as implemented,
#' which assumes duration = end day - start day + 1.
#' \item For the purposes of exceedance detection, any missing temperature values not
#' interpolated over (through optional \code{max_pad_length}) will remain as
#' \code{NA}. This means they will trigger the end of an exceedance if the adjacent
#' temperature values are in exceedance of the \code{threshold}.
#' \item If the function is used to detect consecutive days of temperature under
#' the given \code{theshold}, these temperatures are then taken as being in
#' exceedance below the \code{threshold} as there is no antonym in the English
#' language for 'exceedance'.
#' }
#' This function is based largely on the \code{detect} function found in this
#' package, which was ported from the Python algorithm that was written by Eric
#' Oliver, Institute for Marine and Antarctic Studies, University of Tasmania,
#' Feb 2015, and is documented by Hobday et al. (2016).
#'
#' @return The function will return a list of two components. The first being
#' \code{threshold}, which shows the daily temperatures and on which specific days
#' the given \code{threshold} was exceeded. The second component of the list is
#' \code{exceedance}, which shows a medley of statistics for each discrete group
#' of days in exceedance of the given \code{threshold}. Note that any additional
#' columns left in the data frame given to this function will be output in the
#' \code{threshold} component of the output. For example, if one uses
#' \code{\link{make_whole}} to prepare a time series for analysis and leaves
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
#'   \code{thresh_criterion} are >= \code{min_duration}.}
#'   \item{exceedance}{Boolean indicting if all criteria that define a discrete
#'   group in exceedance of the \code{threshold} are met.}
#'   \item{exceedance_no}{A sequential number indicating the ID and order of
#'   occurence of exceedances.}
#'
#' The individual exceedances are summarised using the following metrics:
#'   \item{index_start}{Row number on which exceedance starts.}
#'   \item{index_stop}{Row number on which exceedance stops.}
#'   \item{exceedance_no}{The same sequential number indicating the ID and
#'   order of the exceedance as found in the \code{threshold} component of the
#'   output list.}
#'   \item{duration}{Duration of exceedance [days].}
#'   \item{date_start}{Start date of exceedance [date].}
#'   \item{date_stop}{Stop date of exceedance [date].}
#'   \item{date_peak}{Date of exceedance peak [date].}
#'   \item{int_mean}{Mean intensity [deg. C].}
#'   \item{int_max}{Maximum (peak) intensity [deg. C].}
#'   \item{int_var}{Intensity variability (standard deviation) [deg. C].}
#'   \item{int_cum}{Cumulative intensity [deg. C x days].}
#'   \item{rate_onset}{Onset rate of exceedance [deg. C / day].}
#'   \item{rate_decline}{Decline rate of exceedance [deg. C / day].}
#'
#' \code{int_max_abs}, \code{int_mean_abs}, \code{int_var_abs}, and
#' \code{int_cum_abs} are as above except as absolute magnitudes
#' rather than relative to the threshold.
#'
#' @author Robert W. Schlegel, Albertus J. Smit
#'
#' @export
#'
#' @examples
#' ts_dat <- make_whole(sst_WA)
#' res <- exceedance(ts_dat, threshold = 25)
#' # show first ten days of daily data:
#' res$threshold[1:10, ]
#' # show first five exceedances:
#' res$exceedance[1:5, ]
exceedance <-
  function(data,
           x = t,
           y = temp,
           threshold = 20,
           below = FALSE,
           min_duration = 5,
           join_across_gaps = TRUE,
           max_gap = 2,
           max_pad_length = 3) {

    temp <- NULL

    ts.x <- eval(substitute(x), data)
    ts.y <- eval(substitute(y), data)
    t_series <- tibble::tibble(ts.x,
                               ts.y)
    rm(ts.x); rm(ts.y)

    t_series$ts.y <- zoo::na.approx(t_series$ts.y, maxgap = max_pad_length)

    if (missing(threshold))
      stop("Oh no! Please provide a threshold against which to calculate exceedances.")

    if (threshold > max(t_series$ts.y, na.rm = T)) {
      stop(paste("The given threshold value of ", threshold, " is greater than the maximum temperature of ",
                 max(t_series$ts.y, na.rm = T), " present in this time series.", sep = ""))
    }

    if (threshold < min(t_series$ts.y, na.rm = T)) {
      stop(paste("The given threshold value of ", threshold, " is less than the minimum temperature of ",
                 min(t_series$ts.y, na.rm = T), " present in this time series.", sep = ""))
    }

    if (below) {
      t_series$ts.y <- -t_series$ts.y
      threshold <- -threshold
    }

    t_series$thresh <- rep(threshold, nrow(t_series))

    t_series$thresh_criterion <- t_series$ts.y >= t_series$thresh
    ex1 <- rle(t_series$thresh_criterion)
    ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
    s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
    proto_exceedances <- s1[ex1$values == TRUE]
    index_stop <- index_start <- NULL ###
    proto_exceedances_rng <-
      lapply(proto_exceedances, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    duration <- NULL ###

    protoFunc <- function(proto_data) {
      out <- proto_data %>%
        dplyr::mutate(duration = index_stop - index_start + 1) %>%
        dplyr::filter(duration >= min_duration) %>%
        dplyr::mutate(date_start = t_series$ts.x[index_start]) %>%
        dplyr::mutate(date_stop = t_series$ts.x[index_stop])
    }

    proto_exceedances <- do.call(rbind, proto_exceedances_rng) %>%
      dplyr::mutate(exceedance_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
      protoFunc()

    if (length(proto_exceedances$index_start) == 0 & below == FALSE) {
      stop(paste("No temperatures over ", threshold, " degrees detected.", sep =  ""))
    }
    if (length(proto_exceedances$index_start) == 0 & below == TRUE) {
      stop(paste("No temperatures under ", threshold, " degrees detected.", sep =  ""))
    }

    t_series$duration_criterion <- rep(FALSE, nrow(t_series))

    for (i in 1:nrow(proto_exceedances)) {
      t_series$duration_criterion[proto_exceedances$index_start[i]:proto_exceedances$index_stop[i]] <-
        rep(TRUE, length = proto_exceedances$duration[i])
    }

    ex2 <- rle(t_series$duration_criterion)
    ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
    s2 <- split(zoo::index(t_series$thresh_criterion), ind2)
    proto_gaps <- s2[ex2$values == FALSE]
    proto_gaps_rng <-
      lapply(proto_gaps, function(x) data.frame(index_start = min(x), index_stop = max(x)))

    proto_gaps <- do.call(rbind, proto_gaps_rng) %>%
      dplyr::mutate(exceedance_no = c(1:length(ex2$values[ex2$values == FALSE]))) %>%
      dplyr::mutate(duration = index_stop - index_start + 1)

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)) {
      proto_gaps <- proto_gaps %>%
        dplyr::mutate(date_start = t_series$ts.x[index_start]) %>%
        dplyr::mutate(date_stop = t_series$ts.x[index_stop]) %>%
        dplyr::filter(duration >= 1 & duration <= max_gap)
    } else {
      join_across_gaps <- FALSE
    }

    if (length(proto_gaps$index_start) == 0) {
      stop(paste("No temperatures in exceedance of ", threshold,
                 " degrees detected for ", min_duration,
                 " or more consecutive days.", sep = ""))
    }

    if (join_across_gaps) {
      t_series$exceedance <- t_series$duration_criterion
      for (i in 1:nrow(proto_gaps)) {
        t_series$exceedance[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <-
          rep(TRUE, length = proto_gaps$duration[i])
      }
    } else {
      t_series$exceedance <- t_series$duration_criterion
    }

    ex3 <- rle(t_series$exceedance)
    ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
    s3 <- split(zoo::index(t_series$exceedance), ind3)
    exceedances <- s3[ex3$values == TRUE]
    exceedance_no <- NULL ###
    exceedances_rng <-
      lapply(exceedances, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    exceedances <- do.call(rbind, exceedances_rng) %>%
      dplyr::mutate(exceedance_no = cumsum(ex3$values[ex3$values == TRUE])) %>%
      protoFunc()

    t_series$exceedance_no <- rep(NA, nrow(t_series))
    for (i in 1:nrow(exceedances)) {
      t_series$exceedance_no[exceedances$index_start[i]:exceedances$index_stop[i]] <-
        rep(i, length = exceedances$duration[i])
    }

    exceedances_list <- plyr::dlply(exceedances, c("exceedance_no"), function(x)
      with(
        t_series,
        data.frame(
          ts.x = c(ts.x[x$index_start:x$index_stop]),
          ts.y = c(ts.y[x$index_start:x$index_stop]),
          thresh = c(thresh[x$index_start:x$index_stop]),
          exceedance_rel_thresh = c(ts.y[x$index_start:x$index_stop]) - c(thresh[x$index_start:x$index_stop])
        )
      )
    )

    thresh <- int_mean <- int_max <- int_cum <- exceedance_rel_thresh <-
      int_mean_abs <- int_max_abs <- int_cum_abs <- ts.y <- NULL ###

    exceedances <- cbind(exceedances,
                         exceedances_list %>%
                           dplyr::bind_rows(.id = "exceedance_no") %>%
                           dplyr::group_by(exceedance_no) %>%
                           dplyr::summarise(date_peak = ts.x[ts.y == max(ts.y)][1],
                                            int_mean = mean(exceedance_rel_thresh),
                                            int_max = max(exceedance_rel_thresh),
                                            int_var = sqrt(stats::var(exceedance_rel_thresh)),
                                            int_cum = max(cumsum(exceedance_rel_thresh)),
                                            int_mean_abs = mean(ts.y),
                                            int_max_abs = max(ts.y),
                                            int_var_abs = sqrt(stats::var(ts.y)),
                                            int_cum_abs = max(cumsum(ts.y))) %>%
                           dplyr::arrange(as.numeric(exceedance_no)) %>%
                           dplyr::select(-exceedance_no))

    exceedance_rel_thresh <- t_series$ts.y - t_series$thresh
    A <- exceedance_rel_thresh[exceedances$index_start]
    B <- t_series$ts.y[exceedances$index_start - 1]
    C <- t_series$thresh[exceedances$index_start - 1]
    if (length(B) + 1 == length(A)) {
      B <- c(NA, B)
      C <- c(NA, C)
    }
    exceedance_rel_thresh_start <- 0.5 * (A + B - C)

    exceedances$rate_onset <- ifelse(
      exceedances$index_start > 1,
      (exceedances$int_max - exceedance_rel_thresh_start) / (as.numeric(
        difftime(exceedances$date_peak, exceedances$date_start, units = "days")) + 0.5),
      NA
    )

    D <- exceedance_rel_thresh[exceedances$index_stop]
    E <- t_series$ts.y[exceedances$index_stop + 1]
    F <- t_series$thresh[exceedances$index_stop + 1]
    exceedance_rel_thresh_end <- 0.5 * (D + E - F)

    exceedances$rate_decline <- ifelse(
      exceedances$index_stop < nrow(t_series),
      (exceedances$int_max - exceedance_rel_thresh_end) / (as.numeric(
        difftime(exceedances$date_stop, exceedances$date_peak, units = "days")) + 0.5),
      NA
    )

    if (below) {
      exceedances <- exceedances %>% dplyr::mutate(
        int_mean = -int_mean,
        int_max = -int_max,
        int_cum = -int_cum,
        int_mean_abs = -int_mean_abs,
        int_max_abs = -int_max_abs,
        int_cum_abs = -int_cum_abs
      )
      t_series <- t_series %>% dplyr::mutate(
        ts.y = -ts.y,
        thresh = -thresh
      )
    }

    names(t_series)[1] <- paste(substitute(x))
    names(t_series)[2] <- paste(substitute(y))

    list(threshold = t_series,
         exceedance = exceedances)
  }
