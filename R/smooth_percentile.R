#' Detect the climatology for a time series.
#'
#' An internal function that helps to create climatologies in
#' accordance with the Hobday et al. (2016) standard.
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{ts2clm}}.
#' @param smoothPercentileWidth The width of the smoothing window
#' to be applied. The default is \code{31} days.
#' @param var_calc This is passed from the ts2clm argument \code{var}
#' and tells the function if a var column exists that needs to be smoothed.
#'
#' @return The function returns the data in the same format it was
#' input as, with the climatology values smoothed as desired.
#'
#' @author Smit, A. J.
#'
smooth_percentile <- function(data, smoothPercentileWidth, var_calc) {

  seas <- thresh <- NULL

  prep <- rbind(utils::tail(data[,-1], smoothPercentileWidth),
                data[,-1],
                utils::head(data[,-1], smoothPercentileWidth))
  rm(data)

  len_clim_year <- 366

  seas <- RcppRoll::roll_mean(as.numeric(prep[,1]), n = smoothPercentileWidth, na.rm = FALSE)
  thresh <- RcppRoll::roll_mean(as.numeric(prep[,2]), n = smoothPercentileWidth, na.rm = FALSE)

  clim <- data.table::data.table(doy = seq_len(len_clim_year),
                                 seas = seas[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)],
                                 thresh = thresh[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)])

  if (var_calc) {
    var <- NULL

    var <- RcppRoll::roll_mean(as.numeric(prep[,3]), n = smoothPercentileWidth, na.rm = FALSE)

    clim$var <- var[(smoothPercentileWidth/2 + 2):((smoothPercentileWidth/2 + 1) + len_clim_year)]
  }
  rm(prep)

  return(clim)
}
