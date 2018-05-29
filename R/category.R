#' Calculate the categories of events.
#'
#' Calculates the categories of a series of events as produced by \code{\link{detect_event}} in
#' accordance with the naming scheme proposed in Hobday et al. (in review).
#'
#' @importFrom dplyr n %>%
#'
#' @param data The function receives the full (list) output from the
#' \code{\link{detect_event}} function.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param S This argument informs the function if the data were collected in the
#' southern hemisphere (TRUE, default) or the northern hemisphere (FALSE) so that it may correctly
#' output the \code{season} column (see below).
#' @param name If a character string (e.g. "Bohai Sea") is provide here it will be used
#' to name the events in the \code{event_name} column (see below) of the output.
#' If no value is provided the default output is "Event".
#'
#' @details An explanation for the categories is as follows:
#' \enumerate{
#'   \item{Moderate}{Events that have been detected, but with a maximum intensity that does not
#'   double the distance between the seasonal climatology and the threshold value.}
#'   \item{Strong}{Events with a maximum intensity that doubles the distance from the seasonal
#'   climatology and the threshold, but do not triple it.}
#'   \item{Severe}{Events that triple the aforementioned distance, but do not quadruple it.}
#'   \item{Extreme}{Events with a maximum intensity that is four times or greater the
#'   aforementioned distance. Scary stuff...}
#'   }
#'
#' @return The function will return a tibble with results similar to those seen in
#' Table 2 of Hobday et al. (in review). This provides the information necessary to
#' appraise the extent of the events in the output of \code{\link{detect_event}} based on the
#' category ranking scale. The category thresholds are calculated based on the difference
#' between the seasonal climatology and threshold climatology produced by
#' \code{\link{detect_event}}. The four category levels are then the difference
#' multiplied by the category level.
#'
#' The definitions for the output columns are as follows:
#'   \item{event_no}{The number of the event as determined by \code{\link{detect_event}}
#'   for reference between the outputs.}
#'   \item{event_name}{The name of the event. Generated from the \code{\link{name}}
#'   value provided and the year of the \code{peak_date} (see following) of
#'   the event. If no \code{\link{name}} value is provided the default "Event" is used.
#'   As proposed in Hobday et al. (in review), \code{Moderate} events are not given a name
#'   so as to prevent multiple repeat names within the same year. If two or more events
#'   ranked greater than Moderate are reported withiin the same year, they will be
#'   differentiated with the addition of a trailing letter
#'   (e.g. Event 2001 a, Event 2001 b). (still in development)}
#'   \item{peak_date}{The date (day) on which the maximum intensity of the event
#'   was recorded.}
#'   \item{category}{The maximum category threshold reached/exceeded by the event.}
#'   \item{i_max}{The maximum intensity of the event above the threshold value.}
#'   \item{duration}{The total duration (days) of the event. Note that this includes
#'   any possible days when the measurement value \code{\link{y}}) may have dropped below the
#'   threshold value. Therefore, the proportion of the event duration (days) spent above
#'   certain thresholds may not add up to 100\% (see following four items).}
#'   \item{p_moderate}{The proportion of the total duration (days) spent above at or
#'   the first threshold, but below any further thresholds.}
#'   \item{p_strong}{The proportion of the total duration (days) spent at or above
#'   the second threshold, but below any further thresholds.}
#'   \item{p_severe}{The proportion of the total duration (days) spent at or above
#'   the third threshold, but below the fourth threshold.}
#'   \item{p_extreme}{The proportion of the total duration (days) spent at or above
#'   the fourth and final threshold. There is currently no recorded event that has
#'   exceeded a hypothetical fifth threshold so none is calculated... yet..}
#'   \item{season}{The season(S) during which the event occurred. If the event
#'   occurred across two seasons this will be displayed as "Winter/Spring".
#'   Across three seasons as "Winter-Summer". Events lasting across four or more
#'   seasons are listed as "Year-round". December (June) is used here as the start of
#'   Austral (Boreal) summer.}
#'
#' @author Robert W. Schlegel
#'
#' @references Hobday et al. (in review). Categorizing and Naming
#' Marine Heatwaves. Oceanography.
#'
#' @export
#'
#' @examples
#' res_WA <- detect_event(ts2clm(sst_WA,
#'                        climatologyPeriod = c("1983-01-01", "2012-12-31")))
#' # Note that the name argument expects a character vector
#' cat_WA <- category(res_WA, name = "WA")
#' tail(cat_WA)
#'
#' # If the data were collected in the northern hemisphere
#' # we must let the funciton know this as seen below
#' res_Med <- detect_event(ts2clm(sst_Med,
#'                         climatologyPeriod = c("1983-01-01", "2012-12-31")))
#' cat_Med <- category(res_Med, S = FALSE, name = "Med")
#' tail(cat_Med)
#'
category <-
  function(data,
           y = temp,
           S = TRUE,
           name = "Event") {

    temp <- NULL

    ts_y <- eval(substitute(y), data$climatology)
    data$climatology$ts_y <- ts_y
    rm(ts_y)

    event_no <- event_name <- peak_date <- category <- duration <- season <- NULL

    cat_frame <- data.frame(event_no = data$event$event_no,
                            event_name = paste0(as.character(name), " ", lubridate::year(data$event$date_peak)),
                            peak_date = data$event$date_peak,
                            category = NA,
                            i_max = round(data$event$intensity_max, 2),
                            duration = data$event$duration)

    seasons <- data.frame(event_no = data$event$event_no,
                          date_start = data$event$date_start,
                          date_end = data$event$date_end,
                          duration = data$event$duration,
                          season = NA)

    ss <- as.POSIXlt(data$event$date_start)
    ss$day <- 1
    ss$mo <- ss$mo + 1

    se <- as.POSIXlt(data$event$date_end)
    se$day <- 1
    se$mo <- se$mo + 1

    start_season <- end_season <- NULL

    if (S) {
      seasons$start_season <- factor(quarters(ss, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Summer", "Fall", "Winter", "Spring"))
      seasons$end_season <- factor(quarters(se, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                    labels = c("Summer", "Fall", "Winter", "Spring"))
    } else {
      seasons$start_season <- factor(quarters(ss, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Winter", "Spring", "Summer", "Fall"))
      seasons$end_season <- factor(quarters(se, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                    labels = c("Winter", "Spring", "Summer", "Fall"))
      }

    seasons <- seasons %>%
      dplyr::mutate(diff_season = as.integer(start_season) - as.integer(end_season))

    for (i in 1:nrow(seasons)) {
      if (seasons$diff_season[i] == 0 & seasons$duration[i] < 100) {
        seasons$season[i] <- paste0(seasons$start_season[i])
      } else if (seasons$diff_season[i] %in% c(-1, 3) & seasons$duration[i] < 180) {
        seasons$season[i] <- paste0(seasons$start_season[i], "/", seasons$end_season[i])
      } else if (seasons$diff_season[i] %in% c(-3, -2, 2)) {
        seasons$season[i] <- paste0(seasons$start_season[i], "-", seasons$end_season[i])
      }
      if (seasons$duration[i] > 270) {
        seasons$season[i] <- "Year-round"
      }
    }

    seas <- thresh <- thresh_2x <- thresh_3x <- thresh_4x <- NULL

    clim_diff <- data$climatology %>%
      dplyr::filter(!is.na(event_no)) %>%
      dplyr::mutate(diff = thresh - seas,
                    thresh_2x = thresh + diff,
                    thresh_3x = thresh_2x + diff,
                    thresh_4x = thresh_3x + diff)

    moderate <- strong <- severe <- extreme <- NULL

    moderate_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(moderate = n()) %>%
      dplyr::ungroup()
    strong_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_2x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(strong = n()) %>%
      dplyr::ungroup()
    severe_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_3x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(severe = n()) %>%
      dplyr::ungroup()
    extreme_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_4x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(extreme = n()) %>%
      dplyr::ungroup()
    cat_n <- dplyr::left_join(moderate_n, strong_n, by = "event_no") %>%
      dplyr::left_join(severe_n, by = "event_no") %>%
      dplyr::left_join(extreme_n, by = "event_no")
    cat_n[is.na(cat_n)] <- 0

    p_moderate <- p_strong <- p_severe <- p_extreme <- NULL

    cat_join <- dplyr::left_join(cat_frame, cat_n, by = "event_no") %>%
      dplyr::mutate(p_moderate = round(((moderate - strong) / duration * 100), 0),
             p_strong = round(((strong - severe) / duration * 100), 0),
             p_severe = round(((severe - extreme) / duration * 100), 0),
             p_extreme = round((extreme / duration * 100), 0),
             category = ifelse(p_extreme > 0, "IV Extreme",
                               ifelse(p_severe > 0, "III Severe",
                                      ifelse(p_strong > 0, "II Strong", "I Moderate"))),
             event_name = replace(event_name, which(category == "I Moderate"), NA)) %>%
      dplyr::arrange(event_no) %>%
      dplyr::left_join(seasons[, c(1,5)], by = "event_no") %>%
      dplyr::select(event_no:duration, p_moderate:season) %>%
      droplevels()

    # RWS: Still need to polish this off
    # Event column touch-ups
    # The idea is to find repeated event names
    # and apend an a, b, c, etc. to the ends
    # in order to make each name unique
    # remove cat 1 names
    # search for duplicates
    # add letter after for all duplicates
    # for(i in 1:levels(unique(cat_res$event_name))){
    #   if(length(levels(cat_res$event_name)[i]) > 1){
    #     for(j in length())
    #   }
    #
    # }

    cat_res <- tibble::as.tibble(cat_join) %>%
      dplyr::arrange(-p_moderate, -p_strong, -p_severe, -p_extreme)
    return(cat_res)
}
