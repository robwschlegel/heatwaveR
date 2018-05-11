#' Detect heatwaves and cold-spells.
#'
#' Applies the Hobday et al. (2016) marine heat wave definition to an input time
#' series of a given value (temperature) along with a daily date vector.
#'
#' @importFrom dplyr %>%
#'
#' @param data A data frame with three columns. In the default setting (i.e. ommitting
#' the arguments \code{doy}, \code{x} and \code{y}; see immediately below), the
#' data set is expected to have the headers \code{doy}, \code{t} and \code{temp}.
#' \code{doy} is the Julian day running from 1 to 366, but modified so that the
#' day-of-year (doy) vector for non-leap-years runs 1...59 and then 61...366.
#' For leap years the 60th day is February 29. The \code{t} column is a vector
#' of dates of class \code{Date}, while \code{temp} is the measured variable (by
#' default it is assumed to be temperature). Data of the appropriate format are
#' created by the function \code{\link{detect_clim}}, but your own data can be supplied
#' if they meet the criteria specified by \code{\link{detect_clim}}.
#' @param doy If a column headed \code{doy} is not available, another column with
#' Julian dates can be supplied. This argument accepts the name of that column. The
#' default name is, of course, \code{doy}.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{make_whole}. If a column headed \code{t} is present in
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param min_duration Minimum duration for acceptance of detected events.
#' Default is \code{5} days.
#' @param join_across_gaps Boolean switch indicating whether to join MHWs which
#' occur before/after a short gap as specified by \code{max_gap}. Default
#' is \code{TRUE}.
#' @param max_gap Maximum length of gap allowed for the joining of MHWs. Default
#' is \code{2} days.
#'
#' @export
detect_event <- function(data,
                        x = t,
                        y = temp,
                        min_duration = 5,
                        join_across_gaps = TRUE,
                        max_gap = 2) {

  if(!(is.numeric(min_duration)))
    stop("Please ensure that 'min_duration' is a numeric/integer value.")
  if(!(is.logical(join_across_gaps)))
    stop("Please ensure that 'join_across_gaps' is either TRUE or FALSE.")
  if(!(is.numeric(max_gap)))
    stop("Please ensure that 'max_gap' is a numeric/integer value.")

  temp <- NULL

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)
  # ts_xy <- tibble::tibble(ts_x, ts_y)
  # rm(ts_x); rm(ts_y)

  t_series <- cbind(data[,1], ts_x, ts_y, data[,4:6])
  t_series$ts_y[is.na(t_series$ts_y)] <- t_series$seas_clim_year[is.na(t_series$ts_y)]
  t_series$thresh_criterion <- t_series$ts_y > t_series$thresh_clim_year

  proto_1 <- proto_event(t_series, criterion_column = 7, min_duration)
  t_series$duration_criterion <- rep(FALSE, nrow(t_series))
  for (i in 1:nrow(proto_1)) {
    t_series$duration_criterion[proto_1$index_start[i]:proto_1$index_stop[i]] <-
      rep(TRUE, length = proto_1$duration[i])
    }

  proto_2 <- proto_event(t_series, criterion_column = 8,
                         min_duration, gaps = TRUE, join_across_gaps)

  ex2 <- rle(t_series$duration_criterion)
  ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
  s2 <- split(zoo::index(t_series$thresh_criterion), ind2)
  proto_gaps <- s2[ex2$values == FALSE]
  proto_gaps_rng <-
    lapply(proto_gaps, function(x) data.frame(index_start = min(x), index_stop = max(x)))

proto_gaps <- do.call(rbind, proto_gaps_rng) %>%
  dplyr::mutate(event_no = c(1:length(ex2$values[ex2$values == FALSE]))) %>%
  dplyr::mutate(duration = index_stop - index_start + 1)

if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)) {
  proto_gaps <- proto_gaps %>%
    dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
    dplyr::mutate(date_stop = t_series$ts_x[index_stop]) %>%
    dplyr::filter(duration >= 1 & duration <= max_gap)
} else {
  join_across_gaps <- FALSE
}

# if (!(any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)))
#   join_across_gaps <- FALSE

if (join_across_gaps) {
  t_series$event <- t_series$duration_criterion
  for (i in 1:nrow(proto_gaps)) {
    t_series$event[proto_gaps$index_start[i]:proto_gaps$index_stop[i]] <-
      rep(TRUE, length = proto_gaps$duration[i])
  }
} else {
  t_series$event <- t_series$duration_criterion
}

ex3 <- rle(t_series$event)
ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
s3 <- split(zoo::index(t_series$event), ind3)
events <- s3[ex3$values == TRUE]
event_no <- NULL
events_rng <-
  lapply(events, function(x)
    data.frame(index_start = min(x), index_stop = max(x)))

events <- do.call(rbind, events_rng) %>%
  dplyr::mutate(event_no = cumsum(ex3$values[ex3$values == TRUE])) %>%
  protoFunc()

t_series$event_no <- rep(NA, nrow(t_series))
for (i in 1:nrow(events)) {
  t_series$event_no[events$index_start[i]:events$index_stop[i]] <-
    rep(i, length = events$duration[i])
}

int_mean <- int_max <- int_cum <- int_mean_rel_thresh <-
  int_max_rel_thresh <- int_cum_rel_thresh <- int_mean_abs <-
  int_max_abs <- int_cum_abs <- int_mean_norm <- int_max_norm <-
  rate_onset <- rate_decline <- mhw_rel_thresh <-
  rel_thresh_norm <- mhw_rel_seas <- NULL

events_list <- plyr::dlply(events, c("event_no"), function(df)
  with(
    t_series,
    data.frame(
      ts_x = c(ts_x[df$index_start:df$index_stop]),
      ts_y = c(ts_y[df$index_start:df$index_stop]),
      seas_clim_year = c(seas_clim_year[df$index_start:df$index_stop]),
      thresh_clim_year = c(thresh_clim_year[df$index_start:df$index_stop]),
      mhw_rel_seas = c(ts_y[df$index_start:df$index_stop]) - c(seas_clim_year[df$index_start:df$index_stop]),
      mhw_rel_thresh = c(ts_y[df$index_start:df$index_stop]) - c(thresh_clim_year[df$index_start:df$index_stop]),
      rel_thresh_norm = c(ts_y[df$index_start:df$index_stop]) - c(thresh_clim_year[df$index_start:df$index_stop]) /
        c(thresh_clim_year[df$index_start:df$index_stop]) - c(seas_clim_year[df$index_start:df$index_stop])
    )
  )
)

events <- cbind(events,
                events_list %>%
                  dplyr::bind_rows(.id = "event_no") %>%
                  dplyr::group_by(event_no) %>%
                  dplyr::summarise(date_peak = ts_x[mhw_rel_seas == max(mhw_rel_seas)][1],
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
                                   int_max_norm = max(rel_thresh_norm)) %>%
                  dplyr::arrange(as.numeric(event_no)) %>%
                  dplyr::select(-event_no))

mhw_rel_seas <- t_series$ts_y - t_series$seas_clim_year
A <- mhw_rel_seas[events$index_start]
B <- t_series$ts_y[events$index_start - 1]
C <- t_series$seas_clim_year[events$index_start - 1]
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
F <- t_series$seas_clim_year[events$index_stop + 1]
mhw_rel_seas_end <- 0.5 * (D + E - F)

events$rate_decline <- ifelse(
  events$index_stop < nrow(t_series),
  (events$int_max - mhw_rel_seas_end) / (as.numeric(
    difftime(events$date_stop, events$date_peak, units = "days")) + 0.5),
  NA
)

if (cold_spells) {
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
  t_series <- t_series %>% dplyr::mutate(
    ts_y = -ts_y,
    seas_clim_year = -seas_clim_year,
    thresh_clim_year = -thresh_clim_year
  )
}

# names(t_series)[1] <- paste(substitute(doy))
names(t_series)[2] <- paste(substitute(x))
names(t_series)[3] <- paste(substitute(y))

list(clim = tibble::as_tibble(t_series),
     event = tibble::as_tibble(events))
}

