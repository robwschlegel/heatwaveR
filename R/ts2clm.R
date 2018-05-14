#' Make a climatology from a daily time series.
#'
#' Creates a daily climatology from a time series of daily temperatures using a
#' user-specified sliding window for the mean and threshold calculation, followed
#' by an optional moving average smoother as used by Hobday et al. (2016).
#'
#' @importFrom dplyr %>%
#'
#' @param data A data frame with three columns. In the default setting (i.e. ommitting
#' the arguments \code{doy}, \code{x} and \code{y}; see immediately below), the
#' data set is expected to have the headers \code{doy}, \code{t} and \code{temp}.
#' \code{doy} is the Julian day running from 1 to 366, but modified so that the
#' day-of-year (doy) vector for non-leap-years runs 1...59 and then 61...366.
#' For leap years the 60th day is February 29. The \code{t} column is a vector
#' of dates of class \code{Date}, while \code{temp} is the measured variable (by
#' default it is assumed to be temperature). Data of the appropriate format are
#' created by the function \code{\link{make_whole}}, but your own data can be supplied
#' if they meet the criteria specified by \code{\link{make_whole}}.
#' @param doy If a column headed \code{doy} is not available, another column with
#' Julian dates can be supplied. This argument accepts the name of that column. The
#' default name is, of course, \code{doy}.
#' @param x This column is expected to contain a vector of dates as per the
#' specification of \code{make_whole}. If a column headed \code{t} is present in
#' the dataframe, this argument may be ommitted; otherwise, specify the name of
#' the column with dates here.
#' @param y This is a column containing the measurement variable. If the column
#' name differs from the default (i.e. \code{temp}), specify the name here.
#' @param clmStart Required. The start date for the period across which
#' the (varying by day-of-year) seasonal cycle and extremes threshold are
#' calculated.
#' @param clmEnd Required. The end date for the period across which
#' the (varying by day-of-year) seasonal cycle and extremes threshold are
#' calculated.
#' @param pctile Threshold percentile (\%) for detection of extreme values.
#' Default is \code{90}th percentile. Please see \code{cold_spells} for more
#' information about the calculation of marine cold-spells.
#' @param windowHalfWidth Width of sliding window about day-of-year (to one
#' side of the center day-of-year) used for the pooling of values and
#' calculation of climatology and threshold percentile. Default is \code{5}
#' days, which gives a window width of 11 days centered on the 6th day of the
#' series of 11 days.
#' @param smoothPercentile Boolean switch selecting whether to smooth the
#' climatology and threshold percentile timeseries with a moving average of
#' \code{smoothPercentileWidth}. Default is \code{TRUE}.
#' @param smoothPercentileWidth Full width of moving average window for smoothing
#' climatology and threshold. The default is \code{31} days.
#' @param maxPadLength Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{maxPadLength} will be left as \code{NA}. Set as an
#' integer. The default is \code{3} days.
#' @param clmOnly Choose to calculate only the climatologies and not the
#' events. The default is \code{FALSE}.
#'
#' @details
#' \enumerate{
#' \item This function assumes that the input time series consists of continuous
#' daily values with few missing values. Time ranges which start and end
#' part-way through the calendar year are supported. The accompanying function
#' \code{\link{make_whole}} aids in the preparation of a time series that is
#' suitable for use with \code{ts2clm}, although this may also be accomplished
#' 'by hand' as long as the criteria are met as discussed in the documentation
#' to \code{\link{make_whole}}.
#' \item It is recommended that a period of at least 30 years is specified in
#' order to produce a climatology that smooths out any decadal thermal
#' periodicities that may be present. It is further advised that full the start
#' and end dates for the climatology period result in full years, e.g.
#' "1982-01-01" to "2011-12-31" or "1982-07-01" to "2012-06-30"; if not, this
#' may result in an unequal weighting of data belonging with certain months
#' within a time series. A daily climatology will be, created; that is, the
#' climatology will be comprised of one mean temperature for each day of the
#' year (365 or 366 days, depending on how leap years are dealt with), and the
#' mean will be based on a sample size that is a function of the length of time
#' determined by \code{clmStart} to \code{clmEnd} and the width of the
#' sliding window specified in \code{windowHalfWidth}.
#' \item This function supports leap years. This is done by ignoring Feb 29s
#' for the initial calculation of the climatology and threshold. The values for
#' Feb 29 are then linearly interpolated from the values for Feb 28 and Mar 1.
#' }
#' The original Python algorithm was written by Eric Oliver, Institute for
#' Marine and Antarctic Studies, University of Tasmania, Feb 2015, and is
#' documented by Hobday et al. (2016).
#'
#' @return The function will return a tibble (see the \code{tidyverse}) with the
#' climatology. The climatology contains the full time series of daily temperatures,
#' as well as the the seasonal climatology and the threshold. The software was
#' designed for creating climatologies of daily temperatures, and the units
#' specified below reflect that intended purpose. However, various other kinds
#' of climatologies may be created, and if that is the case, the appropriate
#' units need to be determined by the user.
#'   \item{doy}{Julian day (day-of-year). For non-leap years it runs 1...59 and
#'   61...366, while leap years run 1...366. This column will be named differently if
#'   another name was specified to the \code{doy} argument.}
#'   \item{seas_clim_year}{Climatological seasonal cycle [deg. C].}
#'   \item{thresh_clim_year}{Seasonally varying threshold (e.g., 90th
#'   percentile) [deg. C].}
#'   \item{var_clim_year}{Seasonally varying variance (standard deviation) [deg. C].}
#' Should \code{clmOnly} be disabled, two additional variables will be included
#' with the climatology, and the climatology replicated over the entire
#' duration of \code{data}.
#'   \item{t}{The date vector in the original time series supplied in \code{data}.}
#'   \item{temp}{The measurement vector as per the the original \code{data} supplied
#'   to the function.}
#'
#' @author Albertus J. Smit, Robert W. Schlegel, Eric C. J. Oliver
#'
#' @references Hobday, A.J. et al. (2016). A hierarchical approach to defining
#' marine heatwaves, Progress in Oceanography, 141, pp. 227-238,
#' doi:10.1016/j.pocean.2015.12.014
#'
#' @export
#'
#' @examples
#' ts_dat <- make_whole(sst_WA)
#' res <- ts2clm(ts_dat, clmStart = "1983-01-01",
#'               clmEnd = "2012-12-31")
#' res
ts2clm <-
  function(data,
           doy = doy,
           x = t,
           y = temp,
           clmStart,
           clmEnd,
           maxPadLength = 3,
           pctile = 90,
           windowHalfWidth = 5,
           smoothPercentile = TRUE,
           smoothPercentileWidth = 31,
           clmOnly = FALSE
  ) {

    temp <- NULL

    doy <- eval(substitute(doy), data)
    ts_x <- eval(substitute(x), data)
    ts_y <- eval(substitute(y), data)
    ts_xy <- tibble::tibble(doy, ts_x, ts_y)
    ts_xy$ts_y <- zoo::na.approx(ts_xy$ts_y, maxgap = maxPadLength)

    rm(doy); rm(ts_x); rm(ts_y)

    if (missing(clmStart))
      stop("Oops! Please provide BOTH start and end dates for the climatology.")

    if (missing(clmEnd))
      stop("Bummer! Please provide BOTH start and end dates for the climatology.")

    clm_start <- clmStart
    if (ts_xy$ts_x[1] > clm_start)
      stop(paste("The specified start date precedes the first day of series, which is ",
                 ts_xy$ts_x[1]))

    clm_end <- clmEnd
    if (clm_end > ts_xy$ts_x[nrow(ts_xy)])
      stop(paste("The specified end date follows the last day of series, which is ",
                 ts_xy$ts_x[nrow(ts_xy)]))

    t_dat <- ts_xy %>%
      dplyr::filter(ts_x >= clm_start & ts_x <= clm_end) %>%
      dplyr::mutate(ts_x = lubridate::year(ts_x)) %>%
      tidyr::spread(ts_x, ts_y)

    t_dat[59:61, ] <- zoo::na.approx(t_dat[59:61, ], maxgap = 1, na.rm = TRUE)
    t_dat <- rbind(utils::tail(t_dat, windowHalfWidth),
                   t_dat, utils::head(t_dat, windowHalfWidth))

    seas_clim_year <- rep(NA, nrow(t_dat))
    thresh_clim_year <- rep(NA, nrow(t_dat))
    var_clim_year <- rep(NA, nrow(t_dat))

    for (i in (windowHalfWidth + 1):((nrow(t_dat) - windowHalfWidth))) {
      seas_clim_year[i] <-
        mean(
          c(t(t_dat[(i - (windowHalfWidth)):(i + windowHalfWidth), 2:ncol(t_dat)])),
          na.rm = TRUE)
      thresh_clim_year[i] <-
        stats::quantile(
          c(t(t_dat[(i - (windowHalfWidth)):(i + windowHalfWidth), 2:ncol(t_dat)])),
          probs = pctile/100,
          type = 7,
          na.rm = TRUE,
          names = FALSE
        )
      var_clim_year[i] <-
        stats::sd(
          c(t(t_dat[(i - (windowHalfWidth)):(i + windowHalfWidth), 2:ncol(t_dat)])),
          na.rm = TRUE
        )
    }

    len_clim_year <- 366
    clm <-
      data.frame(
        doy = t_dat[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year), 1],
        seas_clim_year = seas_clim_year[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)],
        thresh_clim_year = thresh_clim_year[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)],
        var_clim_year = var_clim_year[(windowHalfWidth + 1):((windowHalfWidth) + len_clim_year)]
      )

    if (smoothPercentile) {
      clm <- clm %>%
        dplyr::mutate(
          seas_clim_year = raster::movingFun(
            seas_clim_year,
            n = smoothPercentileWidth,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        ) %>%
        dplyr::mutate(
          thresh_clim_year = raster::movingFun(
            thresh_clim_year,
            n = smoothPercentileWidth,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        ) %>%
        dplyr::mutate(
          var_clim_year = raster::movingFun(
            var_clim_year,
            n = smoothPercentileWidth,
            fun = mean,
            type = "around",
            circular = TRUE,
            na.rm = FALSE
          )
        )
    }

    if (clmOnly) {
      return(clm)
    } else {
      t_series <- ts_xy %>%
        dplyr::inner_join(clm, by = "doy")
      names(t_series)[2] <- paste(substitute(x))
      names(t_series)[3] <- paste(substitute(y))
      return(t_series)
    }
  }
