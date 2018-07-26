#' Spead a time series wide to allow for a climatology to be calculated.
#'
#' An internal function that helps to create a wide time series that will
#' then be used by \code{\link{clim_calc}} within \code{\link{ts2clm}}
#' to produce a climatology as desired by the user.
#'
#' @importFrom data.table :=
#' @importFrom data.table %between%
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
#' @author Smit, A. J.
#'
clim_spread <- function(data, clim_start, clim_end, windowHalfWidth) {

  .NA2mean <- function(x) {
    z <- round(mean(x, na.rm = TRUE), 2)
    x[is.na(x)] <- z
    return(x)
  }

  ts_x <- ts_y <- NULL

  ts_clim <- data[ts_x %between% c(clim_start, clim_end)]
  rm(data)

  data.table::setDT(ts_clim)[, ts_x := format(as.Date(ts_x), "%Y") ]
  ts_spread <- data.table::dcast(ts_clim, doy ~ ts_x, value.var = "ts_y")
  rm(ts_clim)

  ts_spread_filled <- data.table::data.table((sapply(ts_spread[59:61, ],
                                                     function(x) .NA2mean(x))))
  ts_spread[60, ] <- ts_spread_filled[2, ]
  rm(ts_spread_filled)

  begin_pad <- utils::tail(ts_spread, windowHalfWidth)
  end_pad <- utils::head(ts_spread, windowHalfWidth)
  l <- list(begin_pad, ts_spread, end_pad)
  rm(begin_pad); rm(end_pad)

  ts_spread <- data.table::rbindlist(l)
  rm(l)

  len_yr <- length(lubridate::year(clim_start):lubridate::year(clim_end))

  # clim_calc_cpp needs a matrix...
  ts_mat <- as.matrix(ts_spread)[, 2:(len_yr + 1)]

  if (nrow(stats::na.omit(ts_mat)) < nrow(ts_mat)) {
    plugs <- which(is.na(ts_mat), arr.ind = TRUE)
    ts_mat[plugs] <- rowMeans(ts_mat, na.rm = TRUE)[plugs[,1]]
  }

  return(ts_mat)
}
