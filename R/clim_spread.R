#' Spead a time series wide to allow for a climatology to be calculated
#'
#' An internal function that helps to create a wide time series that will
#' then be used by \code{\link{clim_clac}} within \code{\link{detect_clim}}
#' to produce a climatology as desired by the user.
#'
#' @importFrom dplyr %>%
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{detect_clim}}.
#' @param clim_start The first day of the time series to use when spreading.
#' @param clim_end The last day of the time series to use when spreading.
#' @param window_half_width The width of the smoothing window to be applied.
#' This width is doubled and centred around the point that the smoothing
#' occurs. Default = 5, which makes an overall window size of 11.
#'
#' @return The function returns the data in a wide format.
clim_spread <- function(data, clim_start, clim_end, window_half_width){

  ts_spread <- data %>%
    dplyr::filter(ts_x >= clim_start & ts_x <= clim_end) %>%
    dplyr::mutate(ts_x = lubridate::year(ts_x)) %>%
    tidyr::spread(ts_x, ts_y)

  ts_spread[59:61, ] <- zoo::na.approx(ts_spread[59:61, ], maxgap = 1, na.rm = TRUE)
  ts_spread <- rbind(utils::tail(ts_spread, window_half_width),
                     ts_spread,
                     utils::head(ts_spread, window_half_width))

  return(ts_spread)
}
