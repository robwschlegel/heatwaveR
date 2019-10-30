#' Create 'flame' polygons.
#'
#' This function will create polygons between two lines. If given a
#' temperature and theshold time series, like that produced by
#' \code{\link{detect_event}}, the output will meet the specifications
#' of Hobday et al. (2016) shown as 'flame polygons.' If one wishes to
#' plot polygons below a given threshold, and not above, switch the values
#' being fed to the \code{y} and \code{y2} aesthetics. This function differs
#' in use from \code{\link{event_line}} in that it must be created as a
#' \code{ggplot} 'geom' object. The benefit of this being that one may add
#' additional information to the figure as geom layers to ggplot2 graphs
#' as may be necessary.
#'
#' @seealso \code{\link{event_line}} for a non-ggplot2 based flame function.
#'
#' @section Aesthetics:
#' \code{geom_flame} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{y2}}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{size}
#'   \item \code{alpha}
#'   \item \code{linetype}
#' }
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}.
#' If specified and inherit.aes = TRUE (the default), it is combined with the
#' default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified
#' in the call to \code{ggplot()}.
#'
#' A data.frame, or other object, will override the plot data. All objects will
#' be fortified to produce a data frame. See \code{fortify()} for which variables will
#' be created.
#'
#' A function will be called with a single argument, the plot data. The return
#' value must be a \code{data.frame}, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a call
#' to a position adjustment function.
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA},
#' the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and
#' \code{TRUE} always includes. It can also be a named logical vector to finely select
#' the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that define
#' both data and aesthetics and shouldn't inherit behaviour from the default plot
#' specification, e.g. \code{borders()}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param n The number of steps along the x-axis (i.e. in a daily time series this
#' would be days) required before the area between \code{y} and \code{y2} will be
#' filled in. The default of 0 will fill in _all_ of the area between the lines.
#' The standard to match Hobday et al. (2016) is \code{n = 5}.
#' @param n_gap The number of steps along the x-axis (i.e. in a daily time series this
#' would be days) within which to allow \code{geom_flame()} to connect polygons.
#' This is useful when one wants to not screen out parts of a polygon that dip
#' only briefly below \code{y} before coming back up above it. The defauly of 0
#' will not connect any of the polygons. The standard to match
#' Hobday et al. (2016) is \code{n_gap = 2}.
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
#' mhw <- res$clim
#' mhw <- mhw[10580:10690,]
#'
#' library(ggplot2)
#'
#' ggplot(mhw, aes(x = t, y = temp)) +
#'   geom_flame(aes(y2 = thresh)) +
#'   geom_text(aes(x = as.Date("2011-02-01"), y = 28,
#'             label = "That's not a heatwave.\nThis, is a heatwave.")) +
#'   xlab("Date") + ylab(expression(paste("Temperature [", degree, "C]")))
#'
geom_flame <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       n = 0,
                       n_gap = 0,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n = n,
      n_gap = n_gap,
      ...
    )
  )
}
GeomFlame <- ggplot2::ggproto("GeomFlame", ggplot2::Geom,

                              required_aes = c("x", "y", "y2"),

                              default_aes = ggplot2::aes(colour = NA, fill = "salmon",
                                                         size = 0.5, linetype = 1, alpha = NA),

                              draw_key = ggplot2::draw_key_polygon,

                              draw_group = function(data, panel_scales, coord, n, n_gap, na.rm = FALSE) {
                                if (na.rm) data <- data[stats::complete.cases(data[c("x", "y", "y2")]), ]

                                # Check that aesthetics are constant
                                aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
                                if (nrow(aes) > 1) {
                                  stop("Aesthetics must be consistent")
                                }
                                aes <- as.list(aes)

                                # Find events that meet minimum length requirement
                                data_event <- heatwaveR::detect_event(data, x = x, y = y,
                                                                      seasClim = y,
                                                                      threshClim = y2,
                                                                      minDuration = n,
                                                                      maxGap = n_gap,
                                                                      protoEvents = T)

                                # Detect spikes
                                data_event$screen <- base::ifelse(data_event$threshCriterion == FALSE, FALSE,
                                                                  ifelse(data_event$event == FALSE, TRUE, FALSE))

                                # Screen out spikes
                                data <- data[data_event$screen != TRUE,]

                                # Find the ploygon corners
                                x1 <- data$y
                                x2 <- data$y2

                                # Find points where x1 is above x2.
                                above <- x1 > x2
                                above[above == TRUE] <- 1
                                above[is.na(above)] <- 0

                                # Points always intersect when above=TRUE, then FALSE or reverse
                                intersect.points <- which(diff(above) != 0)

                                # Find the slopes for each line segment.
                                x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
                                x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]

                                # Find the intersection for each segment.
                                x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
                                y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))

                                # Coerce x.points to the same scale as x
                                x_gap <- data$x[2] - data$x[1]
                                x.points <- data$x[intersect.points] + (x_gap*(x.points - intersect.points))

                                # Create new data frame and merge to introduce new rows of data
                                data2 <- data.frame(y = c(data$y, y.points), x = c(data$x, x.points))
                                data2 <- data2[order(data2$x),]
                                data <- base::merge(data, data2, by = c("x", "y"), all.y = T)
                                data$y2[is.na(data$y2)] <- data$y[is.na(data$y2)]

                                # Remove missing values for better plotting
                                data$y[data$y < data$y2] <- NA
                                missing_pos <- !stats::complete.cases(data[c("x", "y", "y2")])
                                ids <- cumsum(missing_pos) + 1
                                ids[missing_pos] <- NA

                                positions <- data.frame(x = c(data$x, rev(data$x)),
                                                        y = c(data$y, rev(data$y2)),
                                                        id = c(ids, rev(ids)))
                                munched <- ggplot2::coord_munch(coord, positions, panel_scales)

                                grid::polygonGrob(
                                  munched$x, munched$y, id = munched$id,
                                  default.units = "native",
                                  gp = grid::gpar(
                                    fill = scales::alpha(aes$fill, aes$alpha),
                                    col = aes$colour,
                                    lwd = aes$size * .pt,
                                    lty = aes$linetype)
                                )
                              }
)

#' Visualise a timeline of several event metrics as 'lollipops'.
#'
#' The function will return a graph of the intensity of the selected
#' metric along the *y*-axis versus a time variable along the *x*-axis.
#' The number of top events (\code{n}) from the chosen metric may be highlighted
#' in a brighter colour with the aesthetic value \code{colour_n}.
#' This function differs in use from \code{\link{lolli_plot}}
#' in that it must be created as a ggplot2 'geom' object. The benefit of this being
#' that one may add additional information layer by layer to the figure as
#' geoms as necessary.
#'
#' @seealso \code{\link{lolli_plot}} for a non-geom based lolliplot function.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If
#' specified and inherit.aes = TRUE (the default), it is combined with the
#' default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' \enumerate{
#' \item If NULL, the default, the data is inherited from the plot data as specified
#' in the call to \code{ggplot()}.
#' \item A data.frame, or other object, will override the plot data. All objects will
#' be fortified to produce a data frame. See \code{fortify()} for which variables will
#' be created.
#' \item A function will be called with a single argument, the plot data. The return
#' value must be a \code{data.frame}, and will be used as the layer data.
#' }
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA},
#' the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and
#' \code{TRUE} always includes. It can also be a named logical vector to finely select
#' the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that define
#' both data and aesthetics and shouldn't inherit behaviour from the default plot
#' specification, e.g. \code{borders()}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#' a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#' often aesthetics, used to set an aesthetic to a fixed value, like
#' \code{color = "red"} or \code{size = 3}. They may also be parameters
#' to the paired geom/stat.
#' @param n The number of top events to highlight as based on the value provided
#' to \code{aes(y)}. Default is 0.
#'
#' @section Aesthetics:
#' \code{geom_lolli} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{shape}
#'   \item \code{stroke}
#'   \item \code{fill}
#'   \item \code{colour_n} While this value may be used as an aesthetic, it works
#'   better as a parameter for this function because it is set to use discrete values.
#'   One may provide continuous values to \code{colour_n} but remember that one may
#'   not provide multiple continuous or discrete scales to a single ggplot2 object.
#'   Therefore, if one provides a continuous value to \code{aes(colour)}, the values
#'   supplied to \code{colour_n} must be discrete. \code{ggplot2} will attempt to
#'   do this automatically.
#' }
#'
#' @author Robert W. Schlegel
#'
#' @export
#'
#' @examples
#' ts <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
#' res <- detect_event(ts)
#' mhw <- res$event
#'
#' library(ggplot2)
#'
#' # Height of lollis represent event durations and their colours
#' # are mapped to the events' cumulative intensity:
#' ggplot(mhw, aes(x = date_peak, y = duration)) +
#'   geom_lolli(aes(colour = intensity_cumulative)) +
#'   scale_color_distiller(palette = "Spectral", name = "Cumulative \nintensity") +
#'   xlab("Date") + ylab("Event duration [days]")
#'
#' # Height of lollis represent event durations and the top three (longest)
#' # lollis are highlighted in red:
#' ggplot(mhw, aes(x = date_peak, y = duration)) +
#'   geom_lolli(n = 3, colour_n = "red") +
#'   scale_color_distiller(palette = "Spectral") +
#'   xlab("Peak date") + ylab("Event duration [days]")
#'
#' # Because this is a proper geom, any number of ill-advised things
#' # may be done with it:
#' ggplot(mhw, aes(x = event_no, y = intensity_max)) +
#'   geom_lolli(shape = 5, aes(colour = rate_onset), linetype = "dotted") +
#'   scale_color_distiller(palette = "RdYlGn", name = "Rate \nonset") +
#'   xlab("Event number") + ylab("Max intensity [degree C]")
#'
geom_lolli <- function(mapping = NULL, data = NULL,
                       ...,
                       n = 0,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomLolli,
    data = data,
    mapping = mapping,
    stat = "identity",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n = n,
      ...
    )
  )
}
GeomLolli <- ggplot2::ggproto("GeomLolli", ggplot2::Geom,
                              required_aes = c("x", "y"),
                              default_aes = ggplot2::aes(shape = 19, colour = "grey35", size = 1, fill = NA,
                                                         alpha = NA, stroke = 1, colour_n = "black",
                                                         linetype = "solid"),

                              draw_key = ggplot2::draw_key_point,

                              draw_group = function(data, panel_scales, coord, n) {
                                data$xend = data$x
                                data$yend = 0
                                data = data[order(abs(data$y), decreasing = T),]

                                # Define the big points
                                big_points = data
                                big_points$size = data$size*2

                                # Define the look of the small white fillings
                                small_points = data
                                small_points$size = data$size/2
                                small_points$colour = "white"

                                # Check if the user is trying to highlight more points than are in the data
                                if (n > nrow(data)){
                                  n = nrow(data)
                                }

                                if (n == 0) {
                                  grid::gList(
                                    ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                                    ggplot2::GeomPoint$draw_panel(big_points, panel_scales, coord),
                                    ggplot2::GeomPoint$draw_panel(small_points, panel_scales, coord)
                                  )
                                } else {
                                  # Define the top n events
                                  data_n = data[seq_len(n),]
                                  data_n$colour = data$colour_n[seq_len(n)]
                                  big_points_n = big_points[seq_len(n),]
                                  big_points_n$colour = data$colour_n[seq_len(n)]

                                  grid::gList(
                                    ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                                    ggplot2::GeomPoint$draw_panel(big_points, panel_scales, coord),
                                    ggplot2::GeomSegment$draw_panel(data_n, panel_scales, coord),
                                    ggplot2::GeomPoint$draw_panel(big_points_n, panel_scales, coord),
                                    ggplot2::GeomPoint$draw_panel(small_points, panel_scales, coord)
                                  )
                                }
                              }
)
