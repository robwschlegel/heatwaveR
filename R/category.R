#' Calculate the categories of events.
#'
#' Calculates the categories of MHWs or MCSs produced by \code{\link{detect_event}} in
#' accordance with the naming scheme proposed in Hobday et al. (2018).
#'
#' @importFrom dplyr %>%
#'
#' @param data The function receives the full (list) output from the
#' \code{\link{detect_event}} function.
#' @param y The column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param S This argument informs the function if the data were collected in the
#' southern hemisphere (TRUE, default) or the northern hemisphere (FALSE) so that it may correctly
#' output the \code{season} column (see below).
#' @param name If a character string (e.g. "Bohai Sea") is provided here it will be used
#' to name the events in the \code{event_name} column (see below) of the output.
#' If no value is provided the default output is "Event".
#' @param climatology The default setting of \code{FALSE} will tell this function to output only
#' the summary (wide) results for the individual events as seen in Hobday et al. (2018). If set
#' to \code{TRUE}, this function will return a list of two dataframes, same
#' as \code{\link{detect_event}}. The first dataframe \code{climatology}, contains the same
#' information as found in \code{\link{detect_event}}, but with the addition of the daily
#' intensity (anomaly above seasonal doy threshold) and category values. The second dataframe,
#' \code{event}, is the summary results that this function produces by default.
#' @param season This argument allows the user to decide how the season(s) of occurrence for
#' the MHWs are labelled. The default setting of \code{"range"} will return the range of seasons
#' over which the MHW occurred, as seen in Hobday et al. (2018). One may chose to rather have
#' this function return only the season during the "start", "peak", or "end" of the MHW by giving
#' the corresponding character vector.
#' @param roundVal This argument allows the user to choose how many decimal places
#' the outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundClm = FALSE}. This argument may only be given
#' numeric values or FALSE.
#'
#' @return The function will return a tibble with results similar to those seen in
#' Table 2 of Hobday et al. (2018). This provides the information necessary to
#' appraise the extent of the events in the output of \code{\link{detect_event}} based on the
#' category ranking scale. The category thresholds are calculated based on the difference
#' between the given seasonal climatology and threshold climatology. The four category levels
#' are then the difference multiplied by the category level.
#'
#' The definitions for the default output columns are as follows:
#'   \item{event_no}{The number of the event as determined by \code{\link{detect_event}}
#'   to allow for joining between the outputs.}
#'   \item{event_name}{The name of the event. Generated from the \code{\link{name}}
#'   value provided and the year of the \code{peak_date} (see following) of
#'   the event. If no \code{\link{name}} value is provided the default "Event" is used.
#'   As proposed in Hobday et al. (2018), \code{Moderate} events are not given a name
#'   so as to prevent multiple repeat names within the same year. If two or more events
#'   ranked greater than Moderate are reported within the same year, they will be
#'   differentiated with the addition of a trailing letter
#'   (e.g. Event 2001a, Event 2001b).}
#'   \item{peak_date}{The date (day) on which the maximum intensity of the event
#'   was recorded.}
#'   \item{category}{The maximum category threshold reached/exceeded by the event.}
#'   \item{i_max}{The maximum intensity of the event above the threshold value.}
#'   \item{duration}{The total duration (days) of the event. Note that this includes
#'   any possible days when the measurement value \code{\link{y}}) may have dropped below the
#'   threshold value. Therefore, the proportion of the event duration (days) spent above
#'   certain thresholds may not add up to 100\% (see following four items).}
#'   \item{p_moderate}{The proportion of the total duration (days) spent at or above
#'   the first threshold, but below any further thresholds.}
#'   \item{p_strong}{The proportion of the total duration (days) spent at or above
#'   the second threshold, but below any further thresholds.}
#'   \item{p_severe}{The proportion of the total duration (days) spent at or above
#'   the third threshold, but below the fourth threshold.}
#'   \item{p_extreme}{The proportion of the total duration (days) spent at or above
#'   the fourth and final threshold.}
#'   \item{season}{The season(s) during which the event occurred. If the event
#'   occurred across two seasons this will be displayed as e.g. "Winter/Spring".
#'   Across three seasons as e.g. "Winter-Summer". Events lasting across four or more
#'   seasons are listed as "Year-round". December (June) is used here as the start of
#'   Austral (Boreal) summer. If "start", "peak", or "end" was given to the \code{season}
#'   argument then only the one season during that chosen period will be given.}
#'
#' If \code{climatology = TRUE}, this function will output a list of two dataframes.
#' The first dataframe, \code{climatology}, will contain the following columns:
#'   \item{t}{The column containing the daily date values.}
#'   \item{event_no}{The numeric event number label.}
#'   \item{intensity}{The daily exceedance (default is degrees C) above the
#'   seasonal climatology.}
#'   \item{category}{The category classification per day.}
#'
#' The second dataframe, \code{event}, contains the default output of this function,
#' as detailed above.
#'
#' @details An explanation for the categories is as follows:
#' \enumerate{
#'   \item{I Moderate-}{Events that have been detected, but with a maximum intensity that does not
#'   double the distance between the seasonal climatology and the threshold value.}
#'   \item{II Strong-}{Events with a maximum intensity that doubles the distance from the seasonal
#'   climatology and the threshold, but do not triple it.}
#'   \item{III Severe-}{Events that triple the aforementioned distance, but do not quadruple it.}
#'   \item{IV Extreme-}{Events with a maximum intensity that is four times or greater than the
#'   aforementioned distance. Scary stuff...}
#'   }
#'
#' @author Robert W. Schlegel
#'
#' @references Hobday et al. (2018). Categorizing and Naming
#' Marine Heatwaves. Oceanography 31(2).
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
#' # we must let the function know this, as seen below
#' res_Med <- detect_event(ts2clm(sst_Med,
#'                         climatologyPeriod = c("1983-01-01", "2012-12-31")))
#' cat_Med <- category(res_Med, S = FALSE, name = "Med")
#' tail(cat_Med)
#'
#' # One may also choose to have this function output the daily
#' # category classifications as well by setting: climatology = TRUE
#' cat_WA_daily <- category(res_WA, name = "WA", climatology = TRUE)
#' head(cat_WA_daily$climatology)
#'
#' # Note that this will not return the complete time series, only the
#' # days during which events were detected.
#' # This was done to reduce the size of the output for those working
#' # with gridded data.
#' # Should one want a complete time series, the daily category results
#' # may simply be left_join() with the detect_event() results
#' cat_WA_ts <- dplyr::left_join(res_WA$climatology,
#'                               cat_WA_daily$climatology)
#' head(cat_WA_ts)
#'
category <- function(data,
                     y = temp,
                     S = TRUE,
                     name = "Event",
                     climatology = FALSE,
                     season = "range",
                     roundVal = 4) {

    temp <- NULL

    ts_y <- eval(substitute(y), data$climatology)
    data$climatology$ts_y <- ts_y
    rm(ts_y)

    event_no <- event_name <- peak_date <- category <- duration <- NULL

    if (nrow(stats::na.omit(data$event)) == 0) {
      cat_res <- tibble::as_tibble(data.frame(event_no = NA,
                                              event_name = NA,
                                              peak_date = NA,
                                              category = NA,
                                              i_max = NA,
                                              duration = NA,
                                              p_moderate = NA,
                                              p_strong = NA,
                                              p_severe = NA,
                                              p_extreme = NA,
                                              season = NA)) %>%
        stats::na.omit()
      if (climatology) {
        clim_res <- tibble::as_tibble(data.frame(t = NA,
                                                 event_no = NA,
                                                 intensity = NA,
                                                 category = NA)) %>%
          stats::na.omit()
        res <- list(climatology = clim_res,
                    event = cat_res)
        return(res)
      } else {
        return(cat_res)
        }
      }

    cat_frame <- data.frame(event_no = data$event$event_no,
                            event_name = paste0(as.character(name), " ", lubridate::year(data$event$date_peak)),
                            peak_date = data$event$date_peak,
                            category = NA,
                            i_max = round(data$event$intensity_max, roundVal),
                            duration = data$event$duration)

    seasons <- data.frame(event_no = data$event$event_no,
                          date_start = data$event$date_start,
                          date_peak = data$event$date_peak,
                          date_end = data$event$date_end,
                          duration = data$event$duration,
                          season = NA)

    ss <- as.POSIXlt(data$event$date_start)
    ss$day <- 1
    ss$mo <- ss$mo + 1

    sp <- as.POSIXlt(data$event$date_peak)
    sp$day <- 1
    sp$mo <- sp$mo + 1

    se <- as.POSIXlt(data$event$date_end)
    se$day <- 1
    se$mo <- se$mo + 1

    start_season <- peak_season <- end_season <- NULL

    if (S) {
      seasons$start_season <- factor(quarters(ss, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Summer", "Fall", "Winter", "Spring"))
      seasons$peak_season <- factor(quarters(sp, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Summer", "Fall", "Winter", "Spring"))
      seasons$end_season <- factor(quarters(se, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                    labels = c("Summer", "Fall", "Winter", "Spring"))
    } else {
      seasons$start_season <- factor(quarters(ss, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Winter", "Spring", "Summer", "Fall"))
      seasons$peak_season <- factor(quarters(sp, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                     labels = c("Winter", "Spring", "Summer", "Fall"))
      seasons$end_season <- factor(quarters(se, abbreviate = F), levels = c("Q1", "Q2", "Q3", "Q4"),
                                    labels = c("Winter", "Spring", "Summer", "Fall"))
      }

    if (season == "range") {
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
    } else if (season == "start") {
      seasons$season <- as.character(seasons$start_season)
    } else if (season == "peak") {
      seasons$season <- as.character(seasons$peak_season)
    } else if (season == "end") {
      seasons$season <- as.character(seasons$end_season)
    } else{
      stop("Please provide one of the following to the `season` argument: 'range', 'start', 'peak', 'end'.")
    }

    seas <- thresh <- thresh_2x <- thresh_3x <- thresh_4x <- NULL

    clim_diff <- data$climatology %>%
      dplyr::filter(!is.na(event_no)) %>%
      dplyr::mutate(diff = thresh - seas,
                    thresh_2x = thresh + diff,
                    thresh_3x = thresh_2x + diff,
                    thresh_4x = thresh_3x + diff)

    moderate <- strong <- severe <- extreme <- NULL

    if(max(cat_frame$i_max) < 0){
      clim_diff$ts_y <- -clim_diff$ts_y
      clim_diff$seas <- -clim_diff$seas
      clim_diff$thresh <- -clim_diff$thresh
      clim_diff$thresh_2x <- -clim_diff$thresh_2x
      clim_diff$thresh_3x <- -clim_diff$thresh_3x
      clim_diff$thresh_4x <- -clim_diff$thresh_4x
    }

    moderate_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(moderate = dplyr::n(), .groups = "drop")
    strong_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_2x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(strong = dplyr::n(), .groups = "drop")
    severe_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_3x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(severe = dplyr::n(), .groups = "drop")
    extreme_n <- clim_diff %>%
      dplyr::filter(ts_y >= thresh_4x) %>%
      dplyr::group_by(event_no) %>%
      dplyr::summarise(extreme = dplyr::n(), .groups = "drop")
    cat_n <- dplyr::left_join(moderate_n, strong_n, by = "event_no") %>%
      dplyr::left_join(severe_n, by = "event_no") %>%
      dplyr::left_join(extreme_n, by = "event_no")
    cat_n[is.na(cat_n)] <- 0

    p_moderate <- p_strong <- p_severe <- p_extreme <- event_count <- event_name_letter <- NULL

    cat_join <- dplyr::left_join(cat_frame, cat_n, by = "event_no") %>%
      dplyr::mutate(p_moderate = round(((moderate - strong) / duration * 100), 0),
             p_strong = round(((strong - severe) / duration * 100), 0),
             p_severe = round(((severe - extreme) / duration * 100), 0),
             p_extreme = round((extreme / duration * 100), 0),
             category = ifelse(p_extreme > 0, "IV Extreme",
                               ifelse(p_severe > 0, "III Severe",
                                      ifelse(p_strong > 0, "II Strong", "I Moderate"))),
             event_name = replace(event_name, which(category == "I Moderate"), NA),
             event_name = as.character(event_name)) %>%
      dplyr::arrange(event_no) %>%
      dplyr::left_join(seasons[, c("event_no", "season")], by = "event_no") %>%
      dplyr::select(event_no:duration, p_moderate:season) %>%
      droplevels() %>%
      dplyr::group_by(event_name) %>%
      dplyr::mutate(event_count = dplyr::row_number()) %>%
      dplyr::mutate(event_name_letter = dplyr::case_when(max(event_count) > 1 &
                                                           !is.na(event_name) ~ letters[event_count])) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(event_name = dplyr::case_when(!is.na(event_name_letter) ~ paste0(event_name, event_name_letter), TRUE  ~  event_name),
                    event_name = as.factor(event_name)) %>%
      dplyr::select(-event_count, -event_name_letter)

    cat_res <- tibble::as_tibble(cat_join) %>%
      dplyr::arrange(-p_moderate, -p_strong, -p_severe, -p_extreme)

    if (climatology) {

      doy <- intensity <- NULL

      clim_res <- clim_diff %>%
        dplyr::mutate(category = ifelse(ts_y >= thresh_4x, "IV Extreme",
                                        ifelse(ts_y >= thresh_3x, "III Severe",
                                               ifelse(ts_y >= thresh_2x, "II Strong",
                                                      ifelse(ts_y > thresh, "I Moderate", NA)))),
                      intensity = round(ts_y-seas, roundVal)) %>%
        dplyr::select(t, event_no, intensity, category)

      if(max(cat_frame$i_max) < 0) clim_res$intensity <- -clim_res$intensity

      list(climatology = tibble::as_tibble(clim_res),
           event = cat_res)
    } else {
      return(cat_res)
    }
}

