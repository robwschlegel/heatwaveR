#' Detect the climatology for a time series
#'
#' An internal function that helps to create climatologies in
#' accordance with the Hobday et al. (2016) standard.
#'
#' @importFrom dplyr %>%
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{detect_clim}}.
#' @param smooth_percentile_width The width of the smoothing window
#' to be applied.
#'
#' @return The function returns the data in the same format it was
#' input as, with the climatology values smoothed as desired.
smooth_percentile <- function(data, smooth_percentile_width) {

  clim <- data %>%
    dplyr::mutate(
      seas_clim_year = raster::movingFun(
        seas_clim_year,
        n = smooth_percentile_width,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    ) %>%
    dplyr::mutate(
      thresh_clim_year = raster::movingFun(
        thresh_clim_year,
        n = smooth_percentile_width,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    ) %>%
    dplyr::mutate(
      var_clim_year = raster::movingFun(
        var_clim_year,
        n = smooth_percentile_width,
        fun = mean,
        type = "around",
        circular = TRUE,
        na.rm = FALSE
      )
    )

  return(clim)
}
