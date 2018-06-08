#' Create a line plot of heatwaves or cold-spells.
#'
#' Creates a graph of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
#'
#' @importFrom ggplot2 ggplot aes geom_polygon geom_line scale_colour_manual
#' scale_fill_manual scale_x_date xlab ylab theme theme_grey element_text
#' element_blank element_rect element_line guides guide_legend
#' @importFrom grid unit
#'
#' @param data The function receives the full (list) output from the
#' \code{\link{detect_event}} function.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{make_whole}. If a column headed \code{t} is present in
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param min_duration The minimum duration (days) the event must be for it to
#' qualify as a heatwave or cold-spell.
#' @param spread The number of days leading and trailing the largest event
#' (as per \code{metric}) detected within the time period specified by
#' \code{start_date} and \code{end_date}. The default is 150 days.
#' @param metric This tells the function how to choose the event that should be
#' highlighted as the 'greatest' of the events in the chosen period. One may
#' choose from the following options: \code{intensity_mean}, \code{intensity_max},
#' \code{intensity_var},\code{intensity_cumulative}, \code{intensity_mean_relThresh},
#' \code{intensity_max_relThresh}, \code{intensity_var_relThresh},
#' \code{intensity_cumulative_relThresh}, \code{intensity_mean_abs},
#' \code{intensity_max_abs}, \code{intensity_var_abs}, \code{intensity_cumulative_abs},
#' \code{rate_onset}, \code{rate_decline}. Partial name matching is currently not
#' supported so please specify the metric name precisely. The default is
#' \code{intensity_cumulative}.
#' @param start_date The start date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. This may not necessarily
#' correspond to the biggest event of the specified metric within the entire
#' data set. To plot the biggest event within the whole time series, make sure
#' \code{start_date} and \code{end_date} straddle this event, or simply specify
#' the start and end dates of the full time series given to \code{\link{detect_event}}.
#' @param end_date The end date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. See \code{start_date}
#' for additional information.
#' @param category A boolean choice of TRUE or FALSE. If set to FALSE (default) event_line() will
#' produce a figure as per the second row of Figure 3 in Hobday et al. (2016). If set to TRUE a
#' figure showing the different categories of the MHWs in the chosen period, highlighted as
#' seen in Figure 3 of Hobday et al. (in review), will be produced. If \code{category} = TRUE,
#' \code{metric} will be ignored as a different colouring scheme is used.
#'
#' @return The function will return a line plot indicating the climatology,
#' threshold and temperature, with the hot or cold events that meet the
#' specifications of Hobday et al. (2016) shaded in as appropriate. The plotting
#' of hot or cold events depends on which option is specified in \code{\link{detect_event}}.
#' The top event detect during the selected time period will be visible in a
#' brighter colour. This function differs in use from \code{\link{geom_flame}}
#' in that it creates a stand alone figure. The benefit of this being
#' that one must not have any prior knowledge of ggplot2 to create the figure.
#'
#' @author Robert W. Schlegel
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
#'
#' event_line(res, spread = 100, metric = "intensity_cumulative",
#' start_date = "2010-12-01", end_date = "2011-06-30")
#'
#' event_line(res, spread = 100, start_date = "2010-12-01",
#' end_date = "2011-06-30", category = TRUE)
#'
event_line <- function(data,
                       x = t,
                       y = temp,
                       min_duration = 5,
                       spread = 150,
                       metric = "intensity_cumulative",
                       start_date,
                       end_date,
                       category = FALSE) {

  date_end <- date_start <- duration <-  temp <-  NULL

  if (!(exists("event", data)) | !(exists("climatology", data))) stop("Please ensure you are running this function on the output of 'heatwaveR::detect_event()'")

  ts.x <- eval(substitute(x), data$climatology)
  data$climatology$ts.x <- ts.x
  ts.y <- eval(substitute(y), data$climatology)
  data$climatology$ts.y <- ts.y

  event <- data$event %>%
    dplyr::filter(date_end >= start_date & date_start <= end_date)

  if (nrow(event) == 0) stop("No events detected! Consider changing the 'start_date' or 'end_date' values.")

  if (!(metric %in% c("intensity_mean", "intensity_max", "intensity_var", "intensity_cumulative", "intensity_mean_relThresh", "intensity_max_relThresh",
                      "intensity_var_relThresh","intensity_cumulative_relThresh", "intensity_mean_abs", "intensity_max_abs", "intensity_var_abs",
                      "intensity_cumulative_abs", "rate_onset", "rate_decline"))) {
    stop("Please ensure you have spelled the desired metric correctly.")
  }

  index_start <- index_end <- NULL

  event <- event[order(-abs(event[colnames(event) == metric])),]
  event <- event %>%
    dplyr::filter(duration >= min_duration) %>%
    dplyr::mutate(index_start_fix = index_start - 1,
           index_end_fix = index_end + 1)

  event_top <- event[1, ]

  date_spread <- seq((event_top$date_start - spread), (event_top$date_end + spread), by = "day")

  thresh_2x <- thresh_3x <- thresh_4x <- NULL

  clim_diff <- data$climatology %>%
    dplyr::mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff)

  clim_events <- data.frame()
  for (i in 1:nrow(event)) {
    clim_sub <- clim_diff[(event$index_start_fix[i]):(event$index_end_fix[i]),]
    clim_events <- rbind(clim_events, clim_sub)
  }

  clim_top <- clim_diff[event_top$index_start_fix:event_top$index_end_fix,]

  clim_spread <- clim_diff %>%
    dplyr::filter(ts.x %in% date_spread)

  thresh <- seas <- y1 <- y2 <-  NULL

  if (event_top$intensity_mean > 0) {
    fillCol <- c("events" = "salmon", "peak event" = "red")
    clim_events$y1 <- clim_events$ts.y
    clim_events$y2 <- clim_events$thresh
    clim_top$y1 <- clim_top$ts.y
    clim_top$y2 <- clim_top$thresh
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "navy")
    clim_events$y1 <- clim_events$thresh
    clim_events$y2 <- clim_events$ts.y
    clim_top$y1 <- clim_top$thresh
    clim_top$y2 <- clim_top$ts.y
  }

  ylabel <- expression(paste("Temperautre [", degree, "C]"))

  ep <- ggplot(data = clim_spread, aes(x = ts.x, y = ts.y)) +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y", name = NULL) +
    ylab(ylabel) +
    theme(plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.ticks.length = unit(-0.25, "cm"),
          legend.background = element_rect(colour = "black"),
          legend.direction = "horizontal",
          legend.justification = c(0, 0),
          legend.position = c(0.005, 0.015),
          legend.key = element_blank()
          )

  if(category){

    if(event_top$intensity_mean < 0) stop("Categories currently only calculated for MHWs, not MCSs. But coming soon!")

    lineColCat <- c(
      "Temperature" = "black",
      "Climatology" = "gray20",
      "Threshold" = "darkgreen",
      "2x Threshold" = "darkgreen",
      "3x Threshold" = "darkgreen",
      "4x Threshold" = "darkgreen"
    )

    fillColCat <- c(
      "Moderate" = "#ffc866",
      "Strong" = "#ff6900",
      "Severe" = "#9e0000",
      "Extreme" = "#2d0000"
    )

    ep <- ep +
      geom_flame(data = clim_events, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = y2, fill = "Moderate")) +
      geom_flame(data = clim_events, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = thresh_2x, fill = "Strong")) +
      geom_flame(data = clim_events, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = thresh_3x, fill = "Severe")) +
      geom_flame(data = clim_events, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = thresh_4x, fill = "Extreme")) +
      geom_line(aes(y = thresh_2x, col = "2x Threshold"),
                size = 0.7, linetype = "dashed") +
      geom_line(aes(y = thresh_3x, col = "3x Threshold"),
                size = 0.7, linetype = "dotdash") +
      geom_line(aes(y = thresh_4x, col = "4x Threshold"),
                size = 0.7, linetype = "dotted") +
      geom_line(aes(y = seas, col = "Climatology"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = thresh, col = "Threshold"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = ts.y, col = "Temperature"), size = 0.6) +
      scale_colour_manual(name = NULL, values = lineColCat,
                          breaks = c("Temperature", "Climatology", "Threshold",
                                     "2x Threshold", "3x Threshold", "4x Threshold")) +
      scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
      guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                    "dashed", "dotdash", "dotted")))) +
      theme(legend.direction = "vertical")
    ep

  } else{

    lineCol <- c(
      "Temperature" = "black",
      "Climatology" = "blue",
      "Threshold" = "darkgreen"
    )

    ep <- ep +
      geom_flame(data = clim_events, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = y2, fill = "events")) +
      geom_flame(data = clim_top, size = 0.5,
                 aes(x = ts.x, y = y1, y2 = y2, fill = "peak event")) +
      geom_line(aes(y = seas, col = "Climatology"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = thresh, col = "Threshold"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = ts.y, col = "Temperature"), size = 0.6) +
      scale_colour_manual(name = NULL, values = lineCol,
                          breaks = c("Temperature", "Climatology", "Threshold")) +
      scale_fill_manual(name = NULL, values = fillCol, guide = FALSE)
    ep
  }
}

#' Create a timeline of selected event metrics as 'lollipops'.
#'
#' Visualise a timeline of several possible event metrics as 'lollipop' graphs.
#'
#' @importFrom ggplot2 aes_string geom_segment geom_point scale_x_continuous
#' element_rect element_line labs scale_y_continuous
#' @importFrom grid unit
#'
#' @param data Output from the \code{\link{detect_event}} function.
#' @param xaxis One of \code{event_no}, \code{date_start} or \code{date_peak}.
#' Default is \code{date_start}.
#' @param metric One of \code{intensity_mean}, \code{intensity_max},
#' \code{intensity_cumulative} and \code{duration}.
#'  Default is \code{intensity_max}.
#' @param event_count The number of top events to highlight, as determined by the
#' value given to \code{metric}. Default is 3.
#'
#' @return The function will return a graph of the intensity of the selected
#' \code{metric} along the y-axis and the chosen \code{xaxis} value.
#' The number of top events as per \code{event_count} will be highlighted
#' in a brighter colour. This function differs in use from \code{\link{geom_lolli}}
#' in that it creates a stand-alone figure. The benefit of this being
#' that one must not have any prior knowledge of \code{ggplot2} to create the figure.
#'
#' @author Albertus J. Smit and Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res <- detect_event(ts)
#'
#' library(ggplot2)
#'
#' # The default output
#' lolli_plot(res)
#'
lolli_plot <- function(data,
                       xaxis = "date_peak",
                       metric = "intensity_max",
                       event_count = 3) {

  if (!(exists("event", data)) | !(exists("climatology", data))) stop("Please ensure you are running this function on the output of 'heatwaveR::detect_event()'")

  if (!(metric %in% c("intensity_mean", "intensity_max", "intensity_cumulative", "duration"))) {
    stop("Please ensure you have spelled the desired metric correctly.")
  }

  if (!(xaxis %in% c("event_no", "date_start", "date_peak"))) {
    stop("Please ensure you have spelled the desired xaxis correctly.")
  }

  if (event_count > nrow(data$event)) {
    stop("Please ensure that event_count is less than the total number of events in your results.")
  }

  event <- data$event %>%
    dplyr::select(metric, xaxis)

  y_top <- as.numeric(event[which(abs(event[ ,1]) == max(abs(event[ ,1]))), 1])*1.05
  if (y_top >= 0) y_limits <- c(0, y_top)
  if (y_top < 0) y_limits <- c(y_top, 0)

  if (data$event$intensity_cumulative[1] < 0) {
    lolli_col <- c("steelblue3", "navy")
  } else {
    lolli_col <- c("salmon", "red")
  }

  if (xaxis == "event_no") xlabel <- "Event number"
  if (xaxis == "date_start") xlabel <- "Start date"
  if (xaxis == "date_peak") xlabel <- "Peak date"

  if (metric == "intensity_max") ylabel <- expression(paste("Maximum intensity [", degree, "C]"))
  if (metric == "intensity_mean") ylabel <- expression(paste("Mean intensity [", degree, "C]"))
  if (metric == "intensity_cumulative") ylabel <- expression(paste("Cumulative intensity [", degree, "C x days]"))
  if (metric == "duration") ylabel <- "Duration [days]"

  lolli <- ggplot(data = event, aes_string(x = xaxis, y = metric)) +
    geom_lolli(colour = lolli_col[1], colour_n = lolli_col[2], fill = "grey70", n = event_count) +
    labs(x = xlabel, y = ylabel) +
    scale_y_continuous(expand = c(0, 0), limits = y_limits) +
    theme(plot.background = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
          panel.grid.minor = element_line(colour = NA),
          panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.ticks.length = unit(-0.25, "cm")
          )

  if (xaxis == "event_no") {
    lolli <- lolli +
      scale_x_continuous(breaks = seq(from = 0, to = nrow(data$event), by = 5))
  }
  lolli
}
