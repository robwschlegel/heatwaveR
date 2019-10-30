#' Detect proto-events based on a chosen criterion (column).
#'
#' An internal function that detects the events according to the heatwave
#' definition, and joins across the gaps if desired.
#'
#' @importFrom dplyr %>%
#'
#' @param t_series A dataframe of the correct dimensions inherited
#' from \code{\link{detect_event}} within which this runs.
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
proto_event <- function(t_series,
                        criterion_column,
                        minDuration,
                        joinAcrossGaps,
                        maxGap) {

  index_start <- index_end <- duration <- NULL

  ex1 <- rle(criterion_column)
  ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
  s1 <- split(base::seq_len(nrow(t_series)), ind1)

  proto_events <- do.call(rbind,
                          lapply(s1[ex1$values == TRUE], function(x)
                            data.table::data.table(index_start = min(x),
                                                   index_end = max(x))))
  duration <- proto_events$index_end - proto_events$index_start + 1

  suppressWarnings(
  if (is.null(proto_events) | max(duration) < minDuration){
    res <- data.frame(t_series,
                        durationCriterion = FALSE,
                        event = FALSE,
                        event_no = NA)
      return(res)
  } else {
    proto_events$duration <- duration
  }
  )

  proto_events <- proto_events[duration >= minDuration, ]

  durationCriterion <- rep(FALSE, nrow(t_series))
  for (i in base::seq_len(nrow(proto_events))) {
    durationCriterion[proto_events$index_start[i]:proto_events$index_end[i]] <-
      rep(TRUE, length = proto_events$duration[i])
  }

  if (joinAcrossGaps) {
    ex2 <- rle(durationCriterion)
    ind2 <- rep(seq_along(ex2$lengths), ex2$lengths)
    s2 <- split(base::seq_len(nrow(t_series)), ind2)

    proto_gaps <- do.call(rbind,
                          lapply(s2[ex2$values == FALSE], function(x)
                            data.table::data.table(index_start = min(x), index_end = max(x))))
    proto_gaps$duration <- proto_gaps$index_end - proto_gaps$index_start + 1

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= maxGap)) {
      proto_gaps <- proto_gaps[duration >= 1 & duration <= maxGap, ]

      event <- durationCriterion
      for (i in base::seq_len(nrow(proto_gaps))) {
        event[proto_gaps$index_start[i]:proto_gaps$index_end[i]] <-
          rep(TRUE, length = proto_gaps$duration[i])
      }

    } else {

      event <- durationCriterion

    }

  } else {

    event <- durationCriterion

  }

  ex3  <- rle(event)
  ind3 <- rep(seq_along(ex3$lengths), ex3$lengths)
  s3 <- split(base::seq_len(nrow(t_series)), ind3)

  proto_final <- do.call(rbind,
                          lapply(s3[ex3$values == TRUE], function(x)
                            data.table::data.table(index_start = min(x), index_end = max(x))))
  proto_final$duration <- proto_final$index_end - proto_final$index_start + 1

  event_no <- rep(NA, nrow(t_series))

  for (i in base::seq_len(nrow(proto_final))) {
    event_no[proto_final$index_start[i]:proto_final$index_end[i]] <-
      rep(i, length = proto_final$duration[i])
  }

  res <- cbind(t_series, durationCriterion, event, event_no)

  return(res)

}
