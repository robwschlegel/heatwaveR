#' Create a line plot of heatwaves or cold-spells.
#'
#' Creates a graph of warm or cold events as per the second row of Figure 3 in
#' Hobday et al. (2016).
#'
#' @importFrom ggplot2 ggplot aes geom_polygon geom_line scale_colour_manual
#' scale_fill_manual scale_x_date xlab ylab theme theme_grey element_text
#' element_blank element_rect element_line
#' @importFrom grid unit
#' @importFrom plyr .
#'
#' @param data The function receives the output from the \code{\link{detect}} function.
#' @param min_duration The minimum duration that an event has to for it to
#' qualify as a marine heat wave or marine cold spell.
#' @param spread The the number of days leading and trailing the largest event
#' (as per \code{metric}) detected within the time period specified by
#' \code{start_date} and \code{end_date}. The default is 150 days.
#' @param metric One of the following options: \code{int_mean}, \code{int_max}, \code{int_var},
#' \code{int_cum}, \code{int_mean_rel_thresh}, \code{int_max_rel_thresh}, \code{int_var_rel_thresh},
#' \code{int_cum_rel_thresh}, \code{int_mean_abs}, \code{int_max_abs}, \code{int_var_abs},
#' \code{int_cum_abs}, \code{int_mean_norm}, \code{int_max_norm}, \code{rate_onset}, \code{rate_decline}.
#' Partial name matching is currently not supported so please specify the metric
#' name precisely. The default is \code{int_cum}.
#' @param start_date The start date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. This may not necessarily
#' correspond to the biggest event of the specified metric within the entire
#' data set. To plot the biggest event within the whole time series, make sure
#' \code{start_date} and \code{end_date} straddle this event, or simply specify
#' the start and end dates of the full time series given to \code{\link{detect}}.
#' @param end_date The end date of a period of time within which the largest
#' event (as per \code{metric}) is retrieved and plotted. See \code{start_date}
#' for additional information.
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
#' res <- detect(ts_dat, climatology_start = 1983, climatology_end = 2012) # using default values
#'
#' \dontrun{
#' event_line(res, spread = 200, metric = "int_cum",
#' start_date = "2010-10-01", end_date = "2011-08-30")
#' }
event_line <- function(data,
                       min_duration = 5,
                       spread = 150,
                       metric = "int_cum",
                       start_date = "1999-06-30",
                       end_date = "2000-05-30") {
  date_stop <- date_start <- int_max <- int_mean <- int_cum <- duration <- NULL

  event <- data$event %>%
    dplyr::filter(date_stop >= start_date & date_start <= end_date)
  if (nrow(event) == 0) stop("No events detected!\nConsider changing the 'start_date' or 'end_date' values.")
  event <- event[order(-abs(event[colnames(event) == metric])),]
  event_top <- event[1, ]

  date_spread <- seq((event_top$date_start - spread), (event_top$date_stop + spread), by = 1)

  clim <- dplyr::filter(data$clim, t %in% date_spread)

  temp <- event_no <- thresh_clim_year <- seas_clim_year <- NULL # avoids annoying notes during check...
  dat3 <- data.frame()
  for (i in min(clim$event_no, na.rm = TRUE):max(clim$event_no, na.rm = TRUE)) {
    x <- clim[stats::complete.cases(clim$event_no) & clim$event_no == i,]
    grid.df <-
      data.frame(t = seq(x$t[1], x$t[nrow(x)], by = "day"))
    x <- merge(x, grid.df, by = "t", all.y = TRUE)

    if (nrow(x[x$thresh_criterion != FALSE,]) != nrow(x)) {
      ex1 <- rle(x$thresh_criterion)
      ind1 <- rep(seq_along(ex1$lengths), ex1$lengths)
      s1 <- split(zoo::index(x$thresh_criterion), ind1)
      proto_events <- s1[ex1$values == TRUE]
      index_stop <- index_start <- NULL ###
      proto_events_rng <-
        lapply(proto_events, function(x)
          data.frame(index_start = min(x), index_stop = max(x)))
      duration <- NULL ###
      # min_duration <- NULL ###
      protoFunc <- function(proto_data) {
        out <- proto_data %>%
          dplyr::mutate(duration = index_stop - index_start + 1) %>%
          dplyr::filter(duration >= min_duration) %>%
          dplyr::mutate(date_start = x[index_start, "t"]) %>%
          dplyr::mutate(date_stop = x[index_stop, "t"])
      }
      proto_events <- do.call(rbind, proto_events_rng) %>%
        dplyr::mutate(event_no = cumsum(ex1$values[ex1$values == TRUE])) %>%
        protoFunc()
      sub.event <- function(proto_event){
        df <-  x[proto_event$index_start:proto_event$index_stop,]
        df$event_no_sub <- paste(df$event_no, proto_event$event_no, sep = ".")
        return(df)
      }
      x <- plyr::ddply(proto_events, .(index_start), sub.event)
      x$event_no_sub <- as.character(x$event_no_sub)
    } else {
      event_no_sub <- NULL
      x$event_no_sub <- x$event_no
    }

    mirror <- function(x){
      event_no_sub <- NULL
      y <- data.frame(
        temp = x$temp,
        t = x$t,
        event_no = x$event_no,
        event_no_sub = x$event_no_sub
      )
      z <-
        rbind(y, data.frame(
          temp = rev(x$thresh_clim_year),
          t = rev(x$t),
          event_no = x$event_no,
          event_no_sub = x$event_no_sub
        ))
      z$order <- rep(c(1, 2), each = nrow(x))
      return(z)
    }
    z <- plyr::ddply(x, .(event_no_sub), mirror)
    z$event_no_sub <- as.character(z$event_no_sub)
    dat3 <- rbind(dat3, z)

  }

  lineCol <- c(
    "temperature" = "black",
    "climatology" = "blue",
    "threshold" = "darkgreen"
  )

  if (event_top$int_mean > 0) {
    fillCol <- c("events" = "salmon", "peak event" = "red")
  } else {
    fillCol <- c("events" = "steelblue3", "peak event" = "navy")
  }

  # yaxis = "int_max" yaxis = "int_mean" yaxis = "int_cum" yaxis = "duration"
  if (metric == "int_max") ylabel <- expression(paste("Maximum intensity [", degree, "C]"))
  if (metric == "int_mean") ylabel <- expression(paste("Mean intensity [", degree, "C]"))
  if (metric == "int_cum") ylabel <- expression(paste("Cumulative intensity [", degree, "C x days]"))
  if (metric == "duration") ylabel <- "Duration [days]"
  if (!exists("ylabel")) ylabel <- metric

  ggplot(data = clim, aes(x = t, y = temp)) +
    geom_polygon(data = dat3,
                 aes(x = t, y = temp, group = event_no_sub, fill = "events"), size = 0.5) +
    geom_polygon(data = dat3[dat3$event_no == event_top$event_no[1],],
                 aes(x = t, y = temp, group = event_no_sub, fill = "peak event"),
                 size = 0.5) +
    geom_line(aes(y = seas_clim_year, col = "climatology"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = thresh_clim_year, col = "threshold"),
              size = 0.7, alpha = 1) +
    geom_line(aes(y = temp, col = "temperature"), size = 0.6) +
    scale_colour_manual(name = NULL, values = lineCol) +
    scale_fill_manual(name = NULL, values = fillCol, guide = FALSE) +
    scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
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
}

#' Create a timeline of selected event metrics as 'lollipops'.
#'
#' Visualise a timeline of several event metrics as 'lollipop' graphs.
#'
#' @importFrom ggplot2 aes_string geom_segment geom_point scale_x_continuous
#' element_rect element_line
#'
#' @param data Output from the \code{\link{detect}} function.
#' @param metric One of \code{int_mean}, \code{int_max}, \code{int_cum} and \code{duration}.
#' Default is \code{int_cum}.
#' @param event_count The number of top events to highlight. Default is 3.
#' @param xaxis One of \code{event_no}, \code{date_start} or \code{date_peak}.
#' Default is \code{date_start}.
#'
#' @return The function will return a graph of the intensity of the selected
#' metric along the y-axis versus either \code{t} or \code{event_no}.
#' The number of top events as per \code{event_count} will be highlighted
#' in a brighter colour. This function differs in use from \code{\link{geom_lolli}}
#' in that it creates a stand alone figure. The benefit of this being
#' that one must not have any prior knowledge of ggplot2 to create the figure.
#'
#' @author Albertus J. Smit and Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' ts_dat <- make_whole(sst_NW_Atl)
#' res <- detect(ts_dat, climatology_start = 1983, climatology_end = 2012) # using default values
#'
#' \dontrun{
#' lolli_plot(res, metric = "int_cum", event_count = 3, xaxis = "date_peak")
#' }
lolli_plot <- function(data,
                       metric = "int_max",
                       event_count = 3,
                       xaxis = "date_start") {

  event <- data$event
  if (nrow(event) == 0) stop("No events detected!")

  peak_sort <- NULL
  expr <- lazyeval::interp(~abs(x), x = as.name(metric))
  event <- event %>%
    dplyr::select_("event_no", "date_start", "date_peak", metric) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_(.dots = stats::setNames(list(expr), "peak_sort")) %>%
    dplyr::arrange(dplyr::desc(peak_sort))

  event$col <- "event"
  event[1:event_count, 6] <- "peak event"

  if (event[1, 4] < 0) {
    lolli_col <- c("steelblue3", "navy")
  } else {
    lolli_col <- c("salmon", "red")
  }

  # Create y and x axis labels
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

  # Create the figure
  lolli <- ggplot(data = event, aes_string(x = xaxis, y = metric)) +
    geom_segment(aes_string(xend = xaxis, yend = 0, colour = "col"),
                 size = 0.6, lineend = "butt", show.legend = F) +
    geom_point(aes_string(colour = "col", fill = "col"), shape = 21, size = 2.2) +
    # geom_text(data = event_top, aes_string(label = index, x = xaxis, y = yaxis), size = 2.0) +
    scale_colour_manual(name = NULL, values = lolli_col, guide = FALSE) +
    scale_fill_manual(name = NULL, values = c("ivory1", "grey40"), guide = FALSE) +
    xlab(xlabel) +
    ylab(ylabel) +
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
  if (xaxis == "event_no") {
    lolli <- lolli +
      scale_x_continuous(breaks = seq(from = 0, to = nrow(data$event), by = 5))
  }
  if (event[1, 4] < 0 & metric != "duration") {
    lolli <- lolli +
      theme(legend.justification = c(0, 4.85))
  }
  lolli
}
