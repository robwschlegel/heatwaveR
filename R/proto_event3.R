#' Detect proto-events based on a chosen criterion (column).
#'
#' An internal function that detects the events according to the heatwave
#' definition, and joins across the gaps if desired. This is an update of
#' \code{\link{detect_event}} and is made entirely within data.table to
#' benefit from the speed improvements.
#'
#' @importFrom data.table .I
#'
#' @keywords internal
#'
#' @param p_series A data.table of the correct dimensions received
#' from \code{\link{detect_event3}} within which this runs.
#' @param criterion_column The column to use for the detection of events.
#' @param minDuration Minimum duration for acceptance of detected events.
#' @param joinAcrossGaps This logic gate tells this internal function if
#' it should connect events across the \code{maxGap} (see below). The
#' default it inherits is \code{TRUE}.
#' @param maxGap This is the number of rows (days) across which distinct
#' events will be combined into one event if \code{joinAcrossGaps = TRUE}.
#'
#' @return A dataframe that will be used within \code{\link{detect_event}},
#' or which can be returned by \code{\link{detect_event}} if the switch
#' 'protoEvent' is specified as TRUE.
#'
#' @author Albertus J. Smit, Robert W. Schlegel
#'
proto_event3 <- function(p_series,
                         criterion_column,
                         minDuration,
                         joinAcrossGaps,
                         maxGap) {
  index_start <- index_end <- duration <- grp <-
    durationCriterion <- rleid <- event <- rleid_event <- event_no <- NULL

  p_series[, grp := data.table::rleid(criterion_column)]
  suppressWarnings(proto_events <- p_series[criterion_column == TRUE,
                                            list(index_start = min(.I),
                                                 index_end = max(.I)), by = grp][, grp := NULL])
  p_series[, grp := NULL]

  duration <- proto_events$index_end - proto_events$index_start + 1

  suppressWarnings(if (is.null(proto_events) | nrow(proto_events) == 0 |
                       max(duration) < minDuration) {
    res <- data.table::copy(p_series)
    res[, `:=`(
      durationCriterion = FALSE,
      event = FALSE,
      event_no = NA
    )]
    return(res)
  } else {
    proto_events[, duration := duration]
  })

  proto_events <-
    proto_events[proto_events$duration >= minDuration,]

  p_series[, durationCriterion := FALSE]
  for (i in seq_len(nrow(proto_events))) {
    p_series[proto_events$index_start[i]:proto_events$index_end[i], durationCriterion := TRUE]
  }

  if (joinAcrossGaps) {
    p_series[, rleid := data.table::rleid(durationCriterion)]
    proto_gaps <-
      p_series[durationCriterion == FALSE, list(index_start = min(.I), index_end = max(.I)), by = rleid][, duration := index_end - index_start + 1]
    p_series[, rleid := NULL]
    proto_gaps <-
      proto_gaps[index_end > proto_events$index_start[1]][, rleid := NULL]

    if (any(proto_gaps$duration >= 1 &
            proto_gaps$duration <= maxGap)) {
      proto_gaps <- proto_gaps[duration >= 1 & duration <= maxGap]
      p_series[, event := durationCriterion]

      for (i in seq_len(nrow(proto_gaps))) {
        p_series[proto_gaps$index_start[i]:proto_gaps$index_end[i], event := TRUE]
      }

    } else {
      p_series[, event := durationCriterion]
    }

  } else {
    p_series[, event := durationCriterion]
  }

  p_series[, rleid_event := data.table::rleid(event)]
  proto_final <-
    p_series[event == TRUE, list(index_start = min(.I), index_end = max(.I)), by = rleid_event][, duration := index_end - index_start + 1]
  p_series[, c("rleid_event", "event_no") := list(NULL, NA_integer_)]

  for (i in seq_len(nrow(proto_final))) {
    p_series[proto_final$index_start[i]:proto_final$index_end[i], event_no := i]
  }

  return(p_series)
}
