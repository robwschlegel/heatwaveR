#' Detect proto-events based on a chosen criteria (column)
#'
#' This functions detects proto-events, which are periods of time
#' above a threshold, but without considering a minium starting
#' duration or joining gap.
#'
#' @importFrom dplyr %>%
#'
#' @param t_series A dataframe of the correct diemensions inherited
#' from \code{\link{detect_event}} within which this runs.
#' @param min_duration Minimum duration for acceptance of detected events.
#'
proto_event <- function(t_series,
                        criterion_column,
                        min_duration,
                        gaps = FALSE,
                        join_across_gaps = TRUE){

  ex1 <- rle(t_series[,criterion_column])
  ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
  s1 <- split(zoo::index(t_series$thresh_criterion), ind1)
  proto_value <- s1[ex1$values == TRUE]

  # index_stop <- index_start <- NULL

  proto_rng <-
    lapply(proto_value, function(x)
      data.frame(index_start = min(x), index_stop = max(x)))

  # duration <- NULL

  if(!(gaps)){
    proto_events <- do.call(rbind, proto_rng) %>%
      dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
      dplyr::mutate(duration = index_stop - index_start + 1) %>%
      dplyr::filter(duration >= min_duration) %>%
      dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
      dplyr::mutate(date_stop = t_series$ts_x[index_stop])
    return(proto_events_df)
  } else {
    proto_gaps <- do.call(rbind, proto_rng) %>%
      dplyr::mutate(event_no = c(1:length(ex2$values[ex2$values == FALSE]))) %>%
      dplyr::mutate(duration = index_stop - index_start + 1)

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= max_gap)) {
      proto_gaps <- proto_gaps %>%
        dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
        dplyr::mutate(date_stop = t_series$ts_x[index_stop]) %>%
        dplyr::filter(duration >= 1 & duration <= max_gap)
    }
  }
}





