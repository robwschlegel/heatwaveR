#' Calculate seasonal and threshold climatologies as well as the variance.
#'
#' An internal function that helps to create the climatologies that are
#' then output with \code{\link{ts2clm}}.
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{ts2clm}}.
#' @param windowHalfWidth The width of the smoothing window to be applied.
#' This width is doubled and centred around the point that the smoothing
#' occurs. Default = 5, which makes an overall window size of 11.
#' @param pctile Threshold percentile (\%) for detection of events (MHWs).
#' Default is \code{90}th percentile.
#'
#' @return The function returns the calculated climatologies.
#'
#' @author Smit, A. J.
#'
clim_calc <- function(data, windowHalfWidth, pctile) {

  seas <- rep(NA, nrow(data))
  thresh <- rep(NA, nrow(data))
  var <- rep(NA, nrow(data))

  for (i in (windowHalfWidth + 1):((nrow(data) - windowHalfWidth))) {
    seas[i] <-
      mean(
        c(t(data[(i - (windowHalfWidth)):(i + windowHalfWidth), 1:ncol(data)])),
        na.rm = TRUE)
    thresh[i] <-
      stats::quantile(
        c(t(data[(i - (windowHalfWidth)):(i + windowHalfWidth), 1:ncol(data)])),
        probs = pctile/100,
        type = 7,
        na.rm = TRUE,
        names = FALSE
      )
    var[i] <-
      stats::sd(
        c(t(data[(i - (windowHalfWidth)):(i + windowHalfWidth), 1:ncol(data)])),
        na.rm = TRUE
      )
  }

  len_clim_year <- 366
  doy <- 1:366

  seas <- seas[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)]
  thresh <- thresh[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)]
  var <- var[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)]

  clim <- matrix(c(doy, seas, thresh, var), ncol = 4, byrow = FALSE,
                 dimnames = list(NULL, c("doy", "seas", "thresh", "var")))

  return(clim)
}
