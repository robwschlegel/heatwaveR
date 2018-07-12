#' Pad NA gaps of user-defined width with interpolated values.
#'
#' An internal function that helps to create a time series that will
#' then be used by \code{\link{clim_calc}} within \code{\link{ts2clm}}
#' to produce a climatology as desired by the user.
#'
#' @param doy Date-of-year as per \code{make_whole} or \code{make_whole_fast}.
#' @param x Date as per \code{make_whole} or \code{make_whole_fast}.
#' @param y Measurement variable as per \code{make_whole} or \code{make_whole_fast}.
#' @param maxPadLength Specifies the maximum length of days over which to
#' interpolate (pad) missing data (specified as \code{NA}) in the input
#' temperature time series; i.e., any consecutive blocks of NAs with length
#' greater than \code{maxPadLength} will be left as \code{NA}. Set as an
#' integer. The default is \code{3} days.
#'
#' @return The function returns the data (a data.table) in a long format.
#'
#' @author Smit, A. J.
#'
na_interp <- function(doy = doy,
                      x = ts_x,
                      y = ts_y,
                      maxPadLength = 3) {

  .na_pad <- function(x, fill, maxPadLength) {
    if (maxPadLength <= 0)
      return(x)
    if (maxPadLength >= length(x))
      return(fill)
    naruns <- rle(is.na(x))
    naruns$values[naruns$lengths > maxPadLength] <- FALSE
    naok <- inverse.rle(naruns)
    rm(naruns)
    ifelse(naok, fill, x)
  }

  .na_fun <- function(x, y) {
    na <- is.na(y)
    yi <- stats::approx(x[!na], y[!na], x)$y
    ygap <-
      .na_pad(y, seq_along(y), maxPadLength = maxPadLength)
    ix <- stats::approx(x, seq_along(y), x)$y
    yx <-
      ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]), NA, yi)
    yx
  }

  ts_x <- ts_y <- NULL
  interp_y <- .na_fun(x, y)
  out <- data.table::data.table(doy = doy,
                                ts_x = x,
                                ts_y = interp_y)
  return(out)
}
