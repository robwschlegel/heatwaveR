#' Detect proto-events based on a chosen criterion (column)
#'
#' This functions detects proto-events, which are periods of time
#' above a threshold, but without considering a minimum duration or
#' joining gap.
#'
#' @importFrom dplyr %>%
#'
#' @param t_series A dataframe of the correct dimensions inherited
#' from \code{\link{detect_event}} within which this runs.
#' @param criterion_column The column to use for the detection of events.
#' @param minDuration Minimum duration for acceptance of detected events.
#' @param gaps This logic gate tells this internal function if it should be
#' calculating the lengths of events, or rather the gaps in between them.
#' @param maxGap This is the number of rows (days) across which distinct
#' events will be combined into one event if \code{joinAcrossGaps = TRUE}.
#'
proto_event <- function(t_series,
                        criterion_column,
                        minDuration,
                        gaps = FALSE,
                        maxGap = 2) {


  index_start <- index_stop <- duration <- NULL

  t_series <- as.data.frame(t_series)
  ex1 <- rle(as.vector(t_series[, criterion_column]))
  ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
  s1 <- split(zoo::index(t_series[, criterion_column]), ind1)

  if (!(gaps)) {
    proto_event_value <- s1[ex1$values == TRUE]

    proto_event_rng <-
      lapply(proto_event_value, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    proto_events <- do.call(rbind, proto_event_rng) %>%
      dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
      dplyr::mutate(duration = index_stop - index_start + 1) %>%
      dplyr::filter(duration >= minDuration) %>%
      dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
      dplyr::mutate(date_stop = t_series$ts_x[index_stop])
    return(proto_events)

  } else {
    proto_gap_value <- s1[ex1$values == FALSE]

    proto_gap_rng <-
      lapply(proto_gap_value, function(x)
        data.frame(index_start = min(x), index_stop = max(x)))

    proto_gaps <- do.call(rbind, proto_gap_rng) %>%
      dplyr::mutate(event_no = c(1:length(ex1$values[ex1$values == FALSE]))) %>%
      dplyr::mutate(duration = index_stop - index_start + 1)

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= maxGap)) {
      proto_gaps <- proto_gaps %>%
        dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
        dplyr::mutate(date_stop = t_series$ts_x[index_stop]) %>%
        dplyr::filter(duration >= 1 & duration <= maxGap)
    }
    return(proto_gaps)
  }
}





