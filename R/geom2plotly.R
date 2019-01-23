#' @name plotly_helpers
#' @title Plotly helpers
#' @description This S3 method for GeomFlame allows it to be implemented with plotly::ggplotly()
#'
#' @importFrom utils getFromNamespace
#' @importFrom plotly geom2trace
#'
#' @param data This is a data.frame of information passed to this function
#' from plotly:::layers2traces
#' @param params This is a packet of specific information also passed to this
#' functions from plotly:::layers2traces
#' @param p This is the base plot created by calling ggplot, but is still passed
#' to this functions from plotly:::layers2traces
#'
#' @examples
#' ts_res <- ts2clm(data = sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))
#' ts_res_sub <- ts_res[10500:10800,]
#'
#' library(ggplot2)
#' library(plotly)
#'
#' p <- ggplot(data = ts_res_sub, aes(x = t, y = temp)) +
#'   geom_flame(aes(y2 = thresh), fill = "salmon") +
#'   geom_line(aes(y = temp)) +
#'   geom_line(aes(y = seas), colour = "green") +
#'   geom_line(aes(y = thresh), colour = "red") +
#'   labs(x = "", y = "Temperature (°C)")
#'  plotly::ggplotly(p)
#'
#' @export
geom2trace.GeomFlame <- function (data,
                                  params,
                                  p) {

  # Create data.frame for ease of use
  data1 <- data.frame(x = data[["x"]],
                      y = data[["y"]],
                      y2 = data[["y2"]])

  # Prepare to find the ploygon corners
  x1 <- data1$y
  x2 <- data1$y2

  # # Find points where x1 is above x2.
  above <- x1 > x2
  above[above == TRUE] <- 1
  above[is.na(above)] <- 0

  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points <- which(diff(above) != 0)

  # Find the slopes for each line segment.
  x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]

  # # Find the intersection for each segment.
  x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
  y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))

  # Coerce x.points to the same scale as x
  x_gap <- data1$x[2] - data1$x[1]
  x.points <- data1$x[intersect.points] + (x_gap*(x.points - intersect.points))

  # Create new data frame and merge to introduce new rows of data
  data2 <- data.frame(y = c(data1$y, y.points), x = c(data1$x, x.points))
  data2 <- data2[order(data2$x),]
  data3 <- base::merge(data1, data2, by = c("x","y"), all.y = T)
  data3$y2[is.na(data3$y2)] <- data3$y[is.na(data3$y2)]

  # Remove missing values for better plotting
  data3$y[data3$y < data3$y2] <- NA
  missing_pos <- !stats::complete.cases(data3[c("x", "y", "y2")])
  ids <- cumsum(missing_pos) + 1
  ids[missing_pos] <- NA

  positions <- data.frame(x = c(data3$x, rev(data3$x)),
                          y = c(data3$y, rev(data3$y2)),
                          ids = c(ids, rev(ids)))

  positions <- plotly::group2NA(positions, groupNames = "ids")
  positions <- positions[stats::complete.cases(positions$ids),]
  positions <- dplyr::left_join(positions, data[,-c(2,3)], by = "x")
  # positions$PANEL <- 1
  # positions$group <- -1
  positions$PANEL <- positions$PANEL[stats::complete.cases(positions$PANEL)][1]
  positions$group <- positions$group[stats::complete.cases(positions$group)][1]
  getFromNamespace("geom2trace.GeomPolygon", asNamespace("plotly"))(positions)


  # L <- list(x = positions$x, y = positions$y, text = plotly:::uniq(data[["hovertext"]]),
  #           key = data[["key"]], customdata = data[["customdata"]],
  #           frame = data[["frame"]], ids = positions$ids, type = "scatter",
  #           mode = "lines", line = list(width = plotly:::aes2plotly(data, params, "size"),
  #                                       color = plotly::toRGB(plotly:::aes2plotly(data, params, "colour"),
  #                                                     plotly:::aes2plotly(data, params, "alpha")),
  #                                       dash = plotly:::aes2plotly(data, params, "linetype")),
  #           fill = "toself", fillcolor = plotly::toRGB(plotly:::aes2plotly(data, params, "fill"),
  #                                              plotly:::aes2plotly(data, params, "alpha")),
  #           hoveron = plotly:::hover_on(data))
  # plotly:::compact(L)



  # data$data <- as.list(positions)
  # positions <- as.list(positions)
  # positions$hovertext <- data[["hovertext"]]
  # positions$key <- data[["key"]]
  # positions$custom <- data[["frame"]]
  # positions$frame <- data[["frame"]]
  # data[["x"]] <- positions$x
  # data[["y"]] <- positions$y
  # data[["ids"]] <- positions$ids
  # data$hovertext <- paste0(data$hovertext, "<br>Frequency: ", data[[".wt"]])
  # data$key <- strsplit(data$label, "\\n")
}
