#' Detect the climatology for a time series
#'
#' An internal function that helps to create climatologies in
#' accordance with the Hobday et al. (2016) standard.
#'
#' @importFrom dplyr %>%
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{ts2clm}}.
#' @param smoothPercentileWidth The width of the smoothing window
#' to be applied.
#'
#' @return The function returns the data in the same format it was
#' input as, with the climatology values smoothed as desired.
smooth_percentile <- function(data, smoothPercentileWidth) {

  seas <- thresh <- var <- NULL

  clim <- data %>%
    dplyr::mutate(
      seas = raster::movingFun(
        seas,
        n = smoothPercentileWidth,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    ) %>%
    dplyr::mutate(
      thresh = raster::movingFun(
        thresh,
        n = smoothPercentileWidth,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    ) %>%
    dplyr::mutate(
      var = raster::movingFun(
        var,
        n = smoothPercentileWidth,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    )

  return(clim)
}
