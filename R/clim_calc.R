#' Calculate seasonal and threshold climatologies as wellas the varaince
#'
#' An internal function that helps to create the climatologies that are
#' then output with \code{\link{detect_clim}}.
#'
#' @importFrom dplyr %>%
#'
#' @param data The data given to this function during the calculations
#' performed by \code{\link{detect_clim}}.
#' @param window_half_width The width of the smoothing window to be applied.
#' This width is doubled and centred around the point that the smoothing
#' occurs. Default = 5, which makes an overall window size of 11.
#'
#' @return The function returns the calculated climatologies.
clim_calc <- function(data, window_half_width){

  seas_clim_year <- rep(NA, nrow(data))
  thresh_clim_year <- rep(NA, nrow(data))
  var_clim_year <- rep(NA, nrow(data))

  for (i in (window_half_width + 1):((nrow(data) - window_half_width))) {
    seas_clim_year[i] <-
      mean(
        c(t(data[(i - (window_half_width)):(i + window_half_width), 2:ncol(data)])),
        na.rm = TRUE)
    thresh_clim_year[i] <-
      stats::quantile(
        c(t(data[(i - (window_half_width)):(i + window_half_width), 2:ncol(data)])),
        probs = pctile/100,
        type = 7,
        na.rm = TRUE,
        names = FALSE
      )
    var_clim_year[i] <-
      stats::sd(
        c(t(data[(i - (window_half_width)):(i + window_half_width), 2:ncol(data)])),
        na.rm = TRUE
      )
  }

  len_clim_year <- 366
  clim <-
    data.frame(
      doy = data[(window_half_width + 1):((window_half_width) + len_clim_year), 1],
      seas_clim_year = seas_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
      thresh_clim_year = thresh_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)],
      var_clim_year = var_clim_year[(window_half_width + 1):((window_half_width) + len_clim_year)]
    )

  return(clim)
}
