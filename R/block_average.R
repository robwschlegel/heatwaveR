#' Calculate yearly means for event metrics.
#'
#' @importFrom dplyr %>%
#'
#' @param data Accepts the data returned by the \code{\link{detect_event}} function.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{ts2clm}. If a column headed \code{t} is present in
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param report Specify either \code{full} or \code{partial}. Selecting \code{full} causes
#' the report to contain NAs for any years in which no events were detected
#' (except for \code{count}, which will be zero in those years), while \code{partial}
#' reports only the years wherein events were detected. The default is \code{full}.
#'
#' @details
#' This function needs to be provided with the full output from the \code{detect_event}
#' or \code{exceedance} functions. Note that the yearly averages are calculted only for
#' complete years (i.e. years that start/end part-way through the year at the beginning
#' or end of the original time series are removed from the calculations).
#'
#' This function differs from the python implementation of the function of the
#' same name (i.e., \code{blockAverage}, see \url{https://github.com/ecjoliver/marineHeatWaves})
#' in that we only provide the ability to calculate the average (or aggregate)
#' event metrics in 'blocks' of one year, while the python version allows
#' arbitrary (integer) block sizes.
#'
#' Note that if this function is used on the output of \code{exceedance}, all of the metrics
#' (see below) with \code{relThresh} in the name will be returned as \code{NA} values.
#'
#' @return The function will return a data frame of the averaged (or aggregate)
#' metrics. It includes the following:
#'   \item{year}{The year over which the metrics were averaged.}
#'   \item{count}{The number of events per year.}
#'   \item{duration}{The average duration of events per year [days].}
#'   \item{duration_max}{The maximum duration of an event in each year [days].}
#'   \item{intensity_mean}{The average event "mean intensity" in each year [deg. C].}
#'   \item{intensity_max}{The average event "maximum (peak) intensity" in each year
#'   [deg. C].}
#'   \item{intensity_max_max}{The maximum event "maximum (peak) intensity" in
#'   each year [deg. C].}
#'   \item{intensity_var}{The average event "intensity variability" in each year
#'   [deg. C].}
#'   \item{intensity_cumulative}{The average event "cumulative intensity" in each year
#'   [deg. C x days].}
#'   \item{rate_onset}{Average event onset rate in each year [deg. C / days].}
#'   \item{rate_decline}{Average event decline rate in each year [deg. C / days].}
#'   \item{total_days}{Total number of events days in each year [days].}
#'   \item{total_icum}{Total cumulative intensity over all events in each year [deg. C x days].}
#'
#' \code{intensity_max_relThresh}, \code{intensity_mean_relThresh},
#' \code{intensity_var_relThresh}, and \code{intensity_cumulative_relThresh}
#' are as above except relative to the threshold (e.g., 90th percentile) rather
#' than the seasonal climatology.
#'
#' \code{intensity_max_abs}, \code{intensity_mean_abs}, \code{intensity_var_abs}, and
#' \code{intensity_cumulative_abs} are as above except as absolute magnitudes
#' rather than relative to the seasonal climatology or threshold.
#'
#' @author Albertus J. Smit, Eric C. J. Oliver, Robert W. Schlegel
#'
#' @references Hobday, A.J. et al. (2016), A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi: 10.1016/j.pocean.2015.12.014
#'
#' @export
#'
#' @examples
#' ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res <- detect_event(ts)
#' out <- block_average(res)
#' summary(glm(count ~ year, out, family = "poisson"))
#'
#' library(ggplot2)
#'
#' ggplot(data = out, aes(x = year, y = count)) +
#'   geom_point(colour = "salmon") +
#'   geom_line() +
#'   labs(x = NULL, y = "Number of events")
#'
block_average <- function(data,
                          x = t,
                          y = temp,
                          report = "full") {

  if("climatology" %in% names(data)) {
  clim <- data$climatology
  ts_x <- eval(substitute(x), data$climatology)
  ts_y <- eval(substitute(y), data$climatology)
  event <- data$event
  } else if("threshold" %in% names(data)) {
    clim <- data$threshold
    ts_x <- eval(substitute(x), data$threshold)
    ts_y <- eval(substitute(y), data$threshold)
    event <- data$exceedance
    } else {
      stop("Please ensure you are providing this function with the output of either detect_event() or exceedance().")
    }

  clim$t <- ts_x
  clim$temp <- ts_y

  year <- temp <- date_start <- temp_mean <- temp_min <- temp_max <- NULL

  temp_yr <- clim %>%
    dplyr::group_by(year = lubridate::year(t)) %>%
    dplyr::summarise()

  duration <- duration_max <- count <- intensity_mean <- intensity_max <- intensity_var <- intensity_cumulative <-
    intensity_mean_relThresh <- intensity_max_relThresh <- intensity_var_relThresh <-
    intensity_cumulative_relThresh <- intensity_mean_abs <- intensity_max_abs <- intensity_var_abs <-
    intensity_cumulative_abs <- rate_onset <- rate_decline <- total_days <- total_icum <-
    duration_mean <- intensity_max_mean <- intensity_cumulative_mean <- NULL

  suppressWarnings(
    event_block <- event %>%
      dplyr::group_by(year = lubridate::year(date_start)) %>%
      dplyr::summarise(count = length(duration),
                       duration_mean = mean(duration, na.rm = T),
                       duration_max = max(duration, na.rm = T),
                       intensity_mean = mean(intensity_mean, na.rm = T),
                       intensity_max_mean = mean(intensity_max, na.rm = T),
                       intensity_max_max = max(intensity_max, na.rm = T),
                       intensity_var = mean(intensity_var, na.rm = T),
                       intensity_cumulative_mean = mean(intensity_cumulative, na.rm = T),
                       intensity_mean_relThresh = mean(intensity_mean_relThresh, na.rm = T),
                       intensity_max_relThresh = mean(intensity_max_relThresh, na.rm = T),
                       intensity_var_relThresh = mean(intensity_var_relThresh, na.rm = T),
                       intensity_cumulative_relThresh = mean(intensity_cumulative_relThresh, na.rm = T),
                       intensity_mean_abs = mean(intensity_mean_abs, na.rm = T),
                       intensity_max_abs = mean(intensity_max_abs, na.rm = T),
                       intensity_var_abs = mean(intensity_var_abs, na.rm = T),
                       intensity_cumulative_abs = mean(intensity_cumulative_abs, na.rm = T),
                       rate_onset = mean(rate_onset, na.rm = T),
                       rate_decline = mean(rate_decline, na.rm = T),
                       total_days = sum(duration, na.rm = T),
                       total_icum = sum(intensity_cumulative, na.rm = T)) %>%
      dplyr::rename(duration = duration_mean,
                    intensity_max = intensity_max_mean,
                    intensity_cumulative = intensity_cumulative_mean)
  )

  if (report == "full") {
    event_block <- dplyr::left_join(temp_yr, event_block, by = "year")
    event_block$count[is.na(event_block$count)] <- 0
  } else if (report == "partial") {
    event_block <-
      dplyr::inner_join(temp_yr, event_block, by = "year")
  } else stop("Oops, 'report' must be either 'full' or 'partial'!")

  event_block$count[is.na(event_block$count)] <- 0

  return(event_block)
}
