#' Calculate the categories of events.
#'
#' Calculates the categories of MHWs or MCSs produced by \code{\link{detect_event}} in
#' accordance with the naming scheme proposed in Hobday et al. (2018).
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
#' to \code{TRUE}, this function will return a list of two dataframes.
#' The first dataframe \code{climatology}, contains similar information as found in
#' \code{\link{detect_event}}, with the addition of the daily intensity (anomaly above seasonal doy threshold)
#' and category values, but only reports the days on which an event was detected. The second dataframe,
#' \code{event}, is the summary results that this function produces by default.
#' @param MCScorrect When calculating marine cold-spells (MCSs) it may occur in some areas
#' that the bottom thresholds for the more intense categories will be below -1.8C,
#' this is physically impossible on Earth, so if one wants to correct the bottom thresholds
#' to not be able to exceed -1.8C, set this argument to TRUE (default is FALSE).
#' @param MCSice Sensu Schlegel et al. (2021; Marine cold-spells), it is advisable to classify
#' a MCS with an event threshold below -1.7°C as a 'V Ice' category event.
#' @param season This argument allows the user to decide how the season(s) of occurrence for
#' the MHWs are labelled. The default setting of \code{"range"} will return the range of seasons
#' over which the MHW occurred, as seen in Hobday et al. (2018). One may chose to rather have
#' this function return only the season during the "start", "peak", or "end" of the MHW by giving
#' the corresponding character vector.
#' @param roundVal This argument allows the user to choose how many decimal places
#' the outputs will be rounded to. Default is 4. To
#' prevent rounding set \code{roundClm = FALSE}. This argument may only be given
#' numeric values or FALSE.
#' @param lat_col The user may set \code{lat_col = TRUE} to detect columns named first 'lat',
#' then 'latitude', and use the numeric decimal degree values therein to determine the correct
#' seasons for events. Note that this will override the \code{S} argument. Meaning that if the
#' given/detected latitude column has negative values, \code{S} will automatically be set to
#' \code{TRUE} and vice versa. Also note that if multiple different latitude values are detected
#' this will intentionally cause an error because the \code{category()} function is not meant to be run on more
#' than one time series at once. If latitude is exactly 0, it will be classified as
#' Northern Hemisphere.
#'
#' @return The function will return a data.frame with results similar to those seen in
#' Table 2 of Hobday et al. (2018). This provides the information necessary to
#' appraise the extent of the events in the output of \code{\link{detect_event}} based on the
#' category ranking scale. The category thresholds are calculated based on the difference
#' between the given seasonal climatology and threshold climatology. The four category levels
#' are then the difference multiplied by the category level.
#'
#' The definitions for the default output columns are as follows:
#' \describe{
#'   \item{t}{The column containing the daily date values.}
#'   \item{event_no}{The numeric event number label.}
#'   \item{intensity}{The daily exceedance (default is degrees C) above the
#'   seasonal climatology.}
#'   \item{category}{The category classification per day.}
#'   \item{event_no}{The number of the event as determined by \code{\link{detect_event}}
#'   to allow for joining between the outputs.}
#'   \item{event_name}{The name of the event. Generated from the \code{name}
#'   value provided and the year of the \code{peak_date} (see following) of
#'   the event. If no \code{name} value is provided the default "Event" is used.
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
#'   any possible days when the measurement value \code{y} may have dropped below the
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
#'   }
#'
#' If \code{climatology = TRUE}, this function will output a list of two dataframes.
#' The first dataframe, \code{climatology}, will contain the following columns:
#' \describe{
#'   \item{t}{The column containing the daily date values.}
#'   \item{event_no}{The numeric event number label.}
#'   \item{intensity}{The daily exceedance (default is degrees C) above the
#'   seasonal climatology.}
#'   \item{category}{The category classification per day.}
#'   }
#'
#' The second dataframe, \code{event}, contains the default output of this function,
#' as detailed above.
#'
#' @details An explanation for the categories is as follows:
#' \describe{
#'   \item{I Moderate-}{Events that have been detected, but with a maximum intensity that does not
#'   double the distance between the seasonal climatology and the threshold value.}
#'   \item{II Strong-}{Events with a maximum intensity that doubles the distance from the seasonal
#'   climatology and the threshold, but do not triple it.}
#'   \item{III Severe-}{Events that triple the aforementioned distance, but do not quadruple it.}
#'   \item{IV Extreme-}{Events with a maximum intensity that is four times or greater than the
#'   aforementioned distance.}
#'   \item{V Ice-}{If \code{MCSice = TRUE}, a MCS with an event threshold below -1.7°C will be classified here.}
#'   }
#'
#' @author Robert W. Schlegel
#'
#' @references Hobday et al. (2018). Categorizing and Naming
#' Marine Heatwaves. Oceanography 31(2).
#'
#' @references Schlegel et al. (2021). Marine cold-spells.
#' Progress in Oceanography 198(102684).
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
category <- function(data,
                     y = temp,
                     S = TRUE,
                     name = "Event",
                     climatology = FALSE,
                     MCScorrect = FALSE,
                     MCSice = FALSE,
                     season = "range",
                     roundVal = 4,
                     lat_col = FALSE) {

    temp <- NULL

    ts_y <- eval(substitute(y), data$climatology)
    if (is.null(ts_y) | is.function(ts_y))
      stop("Please ensure that a column named 'temp' is present in your data.frame or that you have assigned a column to the 'y' argument.")
    data$climatology$ts_y <- ts_y
    rm(ts_y)

    event_no <- event_name <- peak_date <- category <- duration <- NULL

    if (nrow(stats::na.omit(data$event)) == 0) {
      cat_res <- data.frame(event_no = NA,
                            event_name = NA,
                            peak_date = NA,
                            category = NA,
                            i_max = NA,
                            duration = NA,
                            p_moderate = NA,
                            p_strong = NA,
                            p_severe = NA,
                            p_extreme = NA,
                            season = NA)
      if (climatology) {
        clim_res <- data.frame(t = NA,
                               event_no = NA,
                               intensity = NA,
                               category = NA)
        res <- list(climatology = clim_res,
                    event = cat_res)
        return(res)
      } else {
        return(cat_res)
      }
    }

    cat_frame <- data.frame(event_no = data$event$event_no,
                            event_name = paste0(as.character(name), " ", format(data$event$date_peak, "%Y")),
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

    ts_lat_col <- start_season <- peak_season <- end_season <- NULL

    if (lat_col) {
      if ("lat" %in% colnames(data$climatology)) {
        # ts_lat_col <- data$climatology[,"lat"]
        ts_lat_col <- eval(substitute(lat), data$climatology)
      } else if ("latitude" %in% colnames(data$climatology)) {
        ts_lat_col <- eval(substitute(latitude), data$climatology)
      } else {
        ts_lat_col <- NULL
      }
    }

    if (!is.null(ts_lat_col[1])) {
      if (!is.numeric(ts_lat_col[1]))
        stop("Please ensure that the latitude values are numeric (i.e. decimal degrees).")
      if (length(unique(ts_lat_col)) > 1)
        stop("Please ensure that only one latitude value is being used per time series.")
      if (ts_lat_col[1] >= 0) S = FALSE
      if (ts_lat_col[1] < 0) S = TRUE
    }

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
      seasons$diff_season <- as.integer(seasons$start_season) - as.integer(seasons$end_season)
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
    } else {
      stop("Please provide one of the following to the `season` argument: 'range', 'start', 'peak', 'end'.")
    }

    seas <- thresh <- thresh_2x <- thresh_3x <- thresh_4x <- NULL

    clim_diff <- data$climatology[!is.na(data$climatology$event_no),]
    clim_diff$diff <- round(clim_diff$thresh - clim_diff$seas, roundVal)
    clim_diff$thresh_2x <- round(clim_diff$thresh + clim_diff$diff, roundVal)
    clim_diff$thresh_3x <- round(clim_diff$thresh_2x + clim_diff$diff, roundVal)
    clim_diff$thresh_4x <- round(clim_diff$thresh_3x + clim_diff$diff, roundVal)
    row.names(clim_diff) <- NULL

    if (min(cat_frame$i_max) < 0) {
      if (MCScorrect) {
        clim_diff$diff <- round(base::ifelse(clim_diff$thresh_4x + clim_diff$diff <= -1.8,
                                             -(clim_diff$thresh + 1.8)/4, clim_diff$diff), roundVal)
        clim_diff$thresh_2x <- round(clim_diff$thresh + clim_diff$diff, roundVal)
        clim_diff$thresh_3x <- round(clim_diff$thresh_2x + clim_diff$diff, roundVal)
        clim_diff$thresh_4x <- round(clim_diff$thresh_3x + clim_diff$diff, roundVal)
      }
    }

    moderate <- strong <- severe <- extreme <- NULL

    if (min(cat_frame$i_max) < 0) {
      clim_diff$ts_y <- -clim_diff$ts_y
      clim_diff$seas <- -clim_diff$seas
      clim_diff$thresh <- -clim_diff$thresh
      clim_diff$thresh_2x <- -clim_diff$thresh_2x
      clim_diff$thresh_3x <- -clim_diff$thresh_3x
      clim_diff$thresh_4x <- -clim_diff$thresh_4x
    }

    moderate_n <- data.frame(base::table(clim_diff[clim_diff$ts_y >= clim_diff$thresh,]$event_no))
    strong_n <- as.data.frame(base::table(clim_diff[clim_diff$ts_y >= clim_diff$thresh_2x,]$event_no))
    severe_n <- as.data.frame(base::table(clim_diff[clim_diff$ts_y >= clim_diff$thresh_3x,]$event_no))
    extreme_n <- as.data.frame(base::table(clim_diff[clim_diff$ts_y >= clim_diff$thresh_4x,]$event_no))
    # if (ncol(moderate_n) == 1) moderate_n <- data.frame(Var1 = NA, Freq = NA)
    if (ncol(strong_n) == 1) strong_n <- data.frame(Var1 = NA, Freq = NA)
    if (ncol(severe_n) == 1) severe_n <- data.frame(Var1 = NA, Freq = NA)
    if (ncol(extreme_n) == 1) extreme_n <- data.frame(Var1 = NA, Freq = NA)
    colnames(moderate_n)[2] <- "moderate"; colnames(strong_n)[2] <- "strong"
    colnames(severe_n)[2] <- "severe"; colnames(extreme_n)[2] <- "extreme"

    event_list <- list(moderate_n, strong_n, severe_n, extreme_n)
    cat_n <- base::Reduce(function(x, y) base::merge(x, y, by = "Var1", all.x = TRUE), event_list)
    cat_n[is.na(cat_n)] <- 0
    colnames(cat_n)[1] <- "event_no"
    cat_n$event_no <- as.integer(cat_n$event_no)

    p_moderate <- p_strong <- p_severe <- p_extreme <- event_count <- event_name_letter <- NULL

    cat_join <- base::merge(cat_frame, cat_n, by = "event_no", all.x = TRUE)
    cat_join$p_moderate <- round(((cat_join$moderate - cat_join$strong) / cat_join$duration * 100), 0)
    cat_join$p_strong <- round(((cat_join$strong - cat_join$severe) / cat_join$duration * 100), 0)
    cat_join$p_severe <- round(((cat_join$severe - cat_join$extreme) / cat_join$duration * 100), 0)
    cat_join$p_extreme <- round((cat_join$extreme / cat_join$duration * 100), 0)
    cat_join$category <- base::ifelse(cat_join$p_extreme > 0, "IV Extreme",
                                      base::ifelse(cat_join$p_severe > 0, "III Severe",
                                                   base::ifelse(cat_join$p_strong > 0, "II Strong", "I Moderate")))
    cat_join$event_name <- base::replace(cat_join$event_name, base::which(cat_join$category == "I Moderate"), NA)
    cat_join <- base::merge(cat_join, seasons[, c("event_no", "season")], by = "event_no", all.x = TRUE)
    cat_join <- cat_join[,c(1:6, 11:15)]
    cat_join$event_count <- as.integer(stats::ave(cat_join$event_name, cat_join$event_name, FUN = seq_along))
    cat_join$event_count_n <- as.integer(stats::ave(cat_join$event_count, cat_join$event_name,  FUN = max))
    cat_join$event_name_letter = base::ifelse(cat_join$event_count_n > 1,
                                              base::ifelse(!is.na(cat_join$event_name), letters[cat_join$event_count], NA), NA)
    cat_join$event_name <- as.factor(base::ifelse(!is.na(cat_join$event_name_letter),
                                                  paste0(cat_join$event_name, cat_join$event_name_letter), cat_join$event_name))
    cat_join <- cat_join[,1:11]

    if (min(cat_frame$i_max) < 0) {
      if (MCSice) {

        max_thresh <- ice_cat <- NULL

        ice_test <- data.frame(event_no = base::unique(clim_diff$event_no),
                               max_thresh = base::tapply(clim_diff$thresh, clim_diff$event_no, max))
        ice_test$ice_cat <- base::ifelse(ice_test$max_thresh > 1.7, TRUE, FALSE)

        cat_join <- base::merge(cat_join, ice_test, by = "event_no", all.x = TRUE)
        cat_join$category <- base::ifelse(cat_join$ice_cat, "V Ice", cat_join$category)
        cat_join <- cat_join[,1:11]
      }
    }

    cat_res <- cat_join[base::order(-cat_join$p_moderate, -cat_join$p_strong,
                                    -cat_join$p_severe, -cat_join$p_extreme),]
    row.names(cat_res) <- NULL

    if (climatology) {

      doy <- intensity <- NULL

      clim_res <- clim_diff
      clim_res$category = base::ifelse(clim_res$ts_y > clim_res$thresh_4x, "IV Extreme",
                                       base::ifelse(clim_res$ts_y > clim_res$thresh_3x, "III Severe",
                                                    base::ifelse(clim_res$ts_y > clim_res$thresh_2x, "II Strong",
                                                                 base::ifelse(clim_res$ts_y > clim_res$thresh, "I Moderate", NA))))
      if (min(cat_frame$i_max) < 0) {
        if (MCSice) {
          clim_res$category <- base::ifelse(clim_res$thresh > 1.7, "V Ice", clim_res$category)
        }
      }
      clim_res$intensity = round(clim_res$ts_y - clim_res$seas, roundVal)
      if (min(cat_frame$i_max) < 0) clim_res$intensity <- -clim_res$intensity
      clim_res <- clim_res[,c("t", "event_no", "intensity", "category")]

      list(climatology = clim_res,
           event = cat_res)
    } else {
      return(cat_res)
    }
}
