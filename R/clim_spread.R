#' Spead a time series wide to allow for a climatology to be calculated.
#'
#' An internal function that helps to create a wide time series that will
#' then be used by \code{\link{clim_calc}} within \code{\link{ts2clm}}
#' to produce a climatology as desired by the user.
#'
#' @importFrom dplyr %>%
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
clim_spread <- function(data, clim_start, clim_end, windowHalfWidth) {

  ts_x <- ts_y <- NULL

  ts_spread <- data %>%
    dplyr::filter(ts_x >= clim_start & ts_x <= clim_end) %>%
    dplyr::mutate(ts_x = lubridate::year(ts_x)) %>%
    tidyr::spread(ts_x, ts_y)

  ts_spread[59:61, ] <- zoo::na.approx(ts_spread[59:61, ],
                                       maxgap = 1, na.rm = TRUE)
  ts_spread <- dplyr::bind_rows(utils::tail(ts_spread, windowHalfWidth),
                                ts_spread,
                                utils::head(ts_spread, windowHalfWidth))

  len_yr <- length(lubridate::year(clim_start):lubridate::year(clim_end))

  ts_mat <- as.matrix(ts_spread)[, 2:(len_yr + 1)]
  return(ts_mat)
}
