#' Detect the climatology for a time series.
#'
#' An internal function that helps to create climatologies in
#' accordance with the Hobday et al. (2016) standard.
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{ts2clm}}.
#' @param smoothPercentileWidth The width of the smoothing window
#' to be applied. The default is \code{31} days.
#'
#' @return The function returns the data in the same format it was
#' input as, with the climatology values smoothed as desired.
#'
#' @author Smit, A. J.
#'
smooth_percentile <- function(data, smoothPercentileWidth) {

  seas <- thresh <- var <- NULL

  clim <- rbind(utils::tail(data[,2:4], smoothPercentileWidth),
                data[,2:4],
                utils::head(data[,2:4], smoothPercentileWidth))
  rm(data)

  len_clim_year <- 366

  seas <- RcppRoll::roll_mean(as.numeric(clim[,1]), n = smoothPercentileWidth, na.rm = FALSE)
  thresh <- RcppRoll::roll_mean(as.numeric(clim[,2]), n = smoothPercentileWidth, na.rm = FALSE)
  var <- RcppRoll::roll_mean(as.numeric(clim[,3]), n = smoothPercentileWidth, na.rm = FALSE)

  clim <- data.table::data.table(doy = 1:len_clim_year,
                                 seas = seas[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)],
                                 thresh = thresh[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)],
                                 var = var[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)])

  return(clim)
}
