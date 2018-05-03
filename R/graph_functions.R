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
#' @param data The function receives the full (list) output from the \code{\link{detect}} function.
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
#' @param metric This tells the function how to choose the event that should be highlighted as the
#' 'greatest' of the events in the chosen period. One may choose from the following options:
#' \code{int_mean}, \code{int_max}, \code{int_var},\code{int_cum}, \code{int_mean_rel_thresh},
#' \code{int_max_rel_thresh}, \code{int_var_rel_thresh},\code{int_cum_rel_thresh}, \code{int_mean_abs},
#' \code{int_max_abs}, \code{int_var_abs}, \code{int_cum_abs}, \code{int_mean_norm}, \code{int_max_norm},
#' \code{rate_onset}, \code{rate_decline}. Partial name matching is currently not supported so please
#' specify the metric name precisely. The default is \code{int_cum}.
#' @param start_date The start date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. This may not necessarily
#' correspond to the biggest event of the specified metric within the entire
#' data set. To plot the biggest event within the whole time series, make sure
#' \code{start_date} and \code{end_date} straddle this event, or simply specify
#' the start and end dates of the full time series given to \code{\link{detect}}.
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
#' of hot or cold events depends on which option is specified in \code{\link{detect}}.
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
#' ts_dat <- make_whole(sst_WA)
#' res <- detect(ts_dat, climatology_start = "1983-01-01",
#'               climatology_end = "2012-12-31")
#'
#' \dontrun{
#' event_line(res, spread = 100, metric = "int_cum",
#' start_date = "2010-12-01", end_date = "2011-06-30")
#'
#' event_line(res, spread = 100, start_date = "2010-12-01",
#' end_date = "2011-06-30", category = TRUE)
#'
#' }
event_line <- function(data,
                       x = t,
                       y = temp,
                       min_duration = 5,
                       spread = 150,
                       metric = "int_cum",
                       start_date,
                       end_date,
                       category = FALSE) {

  date_stop <- date_start <- duration <-  temp <-  NULL

  if (!(is.list(data))) stop("Please ensure you are running this function on the output of 'heatwaveR::detect()'")

  ts.x <- eval(substitute(x), data$clim)
  data$clim$ts.x <- ts.x
  ts.y <- eval(substitute(y), data$clim)
  data$clim$ts.y <- ts.y

  event <- data$event %>%
    dplyr::filter(date_stop >= start_date & date_start <= end_date)

  if (nrow(event) == 0) stop("No events detected!\nConsider changing the 'start_date' or 'end_date' values.")

  if (!(metric %in% c("int_mean", "int_max", "int_var", "int_cum", "int_mean_rel_thresh", "int_max_rel_thresh",
                      "int_var_rel_thresh","int_cum_rel_thresh", "int_mean_abs", "int_max_abs", "int_var_abs",
                      "int_cum_abs", "int_mean_norm", "int_max_norm", "rate_onset", "rate_decline"))) {
    stop("Please ensure you have spelled the desired metric correctly.")
  }

  index_start <- index_stop <- NULL

  event <- event[order(-abs(event[colnames(event) == metric])),]
  event <- event %>%
    dplyr::filter(duration >= min_duration) %>%
    dplyr::mutate(index_start_fix = index_start - 1,
           index_stop_fix = index_stop + 1)

  event_top <- event[1, ]

  date_spread <- seq((event_top$date_start - spread), (event_top$date_stop + spread), by = "day")

  thresh_2x <- thresh_3x <- thresh_4x <- NULL

  clim_diff <- data$clim %>%
    dplyr::mutate(diff = thresh_clim_year - seas_clim_year,
           thresh_2x = thresh_clim_year + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff)

  clim_events <- data.frame()
  for (i in 1:nrow(event)) {
    clim_sub <- clim_diff[(event$index_start_fix[i]):(event$index_stop_fix[i]),]
    clim_events <- rbind(clim_events, clim_sub)
  }

  clim_top <- clim_diff[event_top$index_start_fix:event_top$index_stop_fix,]

  clim_spread <- clim_diff %>%
    dplyr::filter(ts.x %in% date_spread)

  thresh_clim_year <- seas_clim_year <- y1 <- y2 <-  NULL

  if (event_top$int_mean > 0) {
    fillCol <- c("events" = "salmon", "peak event" = "red")
    clim_events$y1 <- clim_events$ts.y
    clim_events$y2 <- clim_events$thresh_clim_year
    clim_top$y1 <- clim_top$ts.y
    clim_top$y2 <- clim_top$thresh_clim_year
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "navy")
    clim_events$y1 <- clim_events$thresh_clim_year
    clim_events$y2 <- clim_events$ts.y
    clim_top$y1 <- clim_top$thresh_clim_year
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

    if(event_top$int_mean < 0) stop("Categories currently only calculated for MHWs, not MCSs. But coming soon!")

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
      geom_line(aes(y = seas_clim_year, col = "Climatology"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = thresh_clim_year, col = "Threshold"),
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
      geom_line(aes(y = seas_clim_year, col = "Climatology"),
                size = 0.7, alpha = 1) +
      geom_line(aes(y = thresh_clim_year, col = "Threshold"),
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
#' element_rect element_line labs
#'
#' @param data Output from the \code{\link{detect}} function.
#' @param metric One of \code{int_mean}, \code{int_max}, \code{int_cum} and \code{duration}.
#' Default is \code{int_cum}.
#' @param event_count The number of top events to highlight. Default is 3.
#' @param xaxis One of \code{event_no}, \code{date_start} or \code{date_peak}.
#' Default is \code{date_start}.
#'
#' @return The function will return a graph of the intensity of the selected
#' \code{metric} along the y-axis versus the choseb \code{xaxis}.
#' The number of top events as per \code{event_count} will be highlighted
#' in a brighter colour. This function differs in use from \code{\link{geom_lolli}}
#' in that it creates a stand alone figure. The benefit of this being
#' that one must not have any prior knowledge of \code{ggplot2} to create the figure.
#'
#' @author Albertus J. Smit and Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' ts_dat <- make_whole(sst_NW_Atl)
#' res <- detect(ts_dat, climatology_start = "1983-01-01",
#'               climatology_end = "2012-12-31")
#'
#' \dontrun{
#' lolli_plot(res, metric = "int_cum", event_count = 3, xaxis = "date_peak")
#' }
lolli_plot <- function(data,
                       metric = "int_max",
                       event_count = 3,
                       xaxis = "date_start") {

  if (!(is.list(data))) stop("Please ensure you are running this function on the output of 'heatwaveR::detect()'")

  if (!(metric %in% c("int_mean", "int_max", "int_cum", "duration"))) {
    stop("Please ensure you have spelled the desired metric correctly.")
  }

  if (!(xaxis %in% c("event_no", "date_start", "date_peak"))) {
    stop("Please ensure you have spelled the desired xaxis correctly.")
  }

  event <- data$event %>%
    dplyr::select(metric, xaxis)

  if (nrow(event) == 0) stop("No events detected!")

  if (event[1, 1] < 0) {
    lolli_col <- c("steelblue3", "navy")
  } else {
    lolli_col <- c("salmon", "red")
  }

  # xaxis = "event_no" xaxis = "date_start" xaxis = "date_peak"
  if (xaxis == "event_no") xlabel <- "Event number"
  if (xaxis == "date_start") xlabel <- "Start date"
  if (xaxis == "date_peak") xlabel <- "Peak date"
  # yaxis = "int_max" yaxis = "int_mean" yaxis = "int_cum" yaxis = "duration"
  if (metric == "int_max") ylabel <- expression(paste("Maximum intensity [", degree, "C]"))
  if (metric == "int_mean") ylabel <- expression(paste("Mean intensity [", degree, "C]"))
  if (metric == "int_cum") ylabel <- expression(paste("Cumulative intensity [", degree, "C x days]"))
  if (metric == "duration") ylabel <- "Duration [days]"
  if (!exists("ylabel")) ylabel <- metric

  lolli <- ggplot(data = event, aes_string(x = xaxis, y = metric)) +
    geom_lolli(colour = lolli_col[1], colour.n = lolli_col[2], fill = "grey70", n = event_count) +
    labs(x = xlabel, y = ylabel) +
    theme(
      plot.background = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
      panel.grid.minor = element_line(colour = NA),
      panel.grid.major = element_line(colour = "black", size = 0.2, linetype = "dotted"),
      axis.text = element_text(colour = "black"),
      axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
      axis.ticks.length = unit(-0.25, "cm")
    )
  if (event_count < 1) {
    lolli <- lolli +
      geom_lolli(colour = lolli_col[1], colour.n = NA, fill = "grey70")
  }
  if (xaxis == "event_no") {
    lolli <- lolli +
      scale_x_continuous(breaks = seq(from = 0, to = nrow(data$event), by = 5))
  }
  if (event[1, 1] < 0 & metric != "duration") {
    lolli <- lolli +
      theme(legend.justification = c(0, 4.85))
  }
  lolli
}
