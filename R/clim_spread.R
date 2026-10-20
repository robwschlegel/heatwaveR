#' Spread a time series wide to allow for a climatology to be calculated.
#'
#' An internal function that helps to create a wide time series that will
#' then be used by \code{\link{clim_calc}} within \code{\link{ts2clm}}
#' to produce a climatology as desired by the user.
#'
#' @keywords internal
#'
#' @importFrom data.table := %between%
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{ts2clm}}.
#' @param clim_start The first day of the time series to use when spreading.
#' @param clim_end The last day of the time series to use when spreading.
#' @param windowHalfWidth The width of the smoothing window to be applied.
#' This width is doubled and centred around the point that the smoothing
#' occurs. Default = 5, which makes an overall window size of 11.
#'
#' @return The function returns the data (a matrix) in a wide format.
#'
#' @author Smit, A. J., Villeneuve, A., Schlegel, R. W.
#'
clim_spread <- function(data, clim_start, clim_end, windowHalfWidth) {

  .NA2mean <- function(x) {
    z <- round(mean(x, na.rm = TRUE), 2)
    x[is.na(x)] <- z
    return(x)
  }

  # testing...
  # data <- ts_whole

  ts_x <- ts_y <- NULL

  if("hoy" %in% colnames(data)){
    clim_start_hour <- as.POSIXct(paste0(clim_start," 00:00:00"))
    clim_end_hour <- as.POSIXct(paste0(clim_end," 23:00:00"))
    ts_clim <- data[data$ts_x >= clim_start_hour, ]
    ts_clim <- ts_clim[ts_clim$ts_x <= clim_end_hour, ]
  } else {
    ts_clim <- data.table::as.data.table(data)[ts_x %between% c(clim_start, clim_end)]
  }
  rm(data)

  data.table::setDT(ts_clim)[, ts_x := format(ts_x, "%Y") ]
  if("hoy" %in% colnames(ts_clim)){
    ts_spread <- data.table::dcast(ts_clim, hoy ~ ts_x, value.var = "ts_y", mean)
  } else {
    ts_spread <- data.table::dcast(ts_clim, doy ~ ts_x, value.var = "ts_y", mean)
  }
  rm(ts_clim)

  if("hoy" %in% colnames(ts_spread)){
    ts_spread_filled <- data.table::data.table((sapply(ts_spread[1416:1441, ],
                                                       function(x) .NA2mean(x))))
    ts_spread[1417:1440, ] <- ts_spread_filled[2:25, ]
  } else {
    ts_spread_filled <- data.table::data.table((sapply(ts_spread[59:61, ],
                                                       function(x) .NA2mean(x))))
    ts_spread[60, ] <- ts_spread_filled[2, ]
  }
  rm(ts_spread_filled)

  len_yr <- length(as.integer(substr(clim_start, 1, 4)):as.integer(substr(clim_end, 1, 4)))

  # clim_calc_cpp needs a matrix...
  ts_mat <- as.matrix(ts_spread)[, 2:(len_yr + 1)]
  rm(ts_spread)

  if (nrow(stats::na.omit(ts_mat)) < nrow(ts_mat)) {
    plugs <- which(is.na(ts_mat), arr.ind = TRUE)
    ts_mat[plugs] <- rowMeans(ts_mat, na.rm = TRUE)[plugs[,1]]
  }

  ts_mat <- clim_pad(ts_mat, windowHalfWidth)

  return(ts_mat)
}

#' Pad a wide time series with the neighbouring days of the adjacent years.
#'
#' An internal function that adds \code{windowHalfWidth} rows before and after
#' the wide time series created by \code{\link{clim_spread}}, so that the
#' sliding window of \code{\link{clim_calc}} can be centred on the first and
#' last days of the year. The rows before the first day of each year are the
#' last days of the previous year, and the rows after the last day of each
#' year are the first days of the next year, as they are in the time series.
#' The first and last years of the climatology period have no such
#' neighbours, and receive \code{NA}, which \code{\link{clim_calc}} skips.
#'
#' @keywords internal
#'
#' @param ts_mat The wide time series (a matrix, one column per year).
#' @param windowHalfWidth The half width of the sliding window.
#'
#' @return The padded matrix, with \code{2 * windowHalfWidth} more rows.
#'
#' @author Schlegel, R. W.
#'
clim_pad <- function(ts_mat, windowHalfWidth) {
  n_row <- nrow(ts_mat)
  n_col <- ncol(ts_mat)
  w <- seq_len(windowHalfWidth)
  begin_pad <- cbind(NA, ts_mat[n_row - windowHalfWidth + w, -n_col, drop = FALSE])
  end_pad <- cbind(ts_mat[w, -1, drop = FALSE], NA)
  padded <- rbind(begin_pad, ts_mat, end_pad, deparse.level = 0)
  dimnames(padded) <- list(NULL, colnames(ts_mat))
  return(padded)
}
