#' NOAA Optimally Interpolated (OI) v2.1 daily 1/4 degree SST for the NW Atlantic region.
#'
#' A dataset containing the sea surface temperature (in degrees Celsius)
#' and date for the Northwest Atlantic region from 1982-01-01 to 2020-12-31.
#'
#' lon/lat: -66.875/43.125
#'
#' @format A dataframe with 14245 rows and 2 variables:
#' \describe{
#'   \item{t}{date, as.Date() format}
#'   \item{temp}{SST, in degrees Celsius}
#'   ...
#' }
#' @source \url{https://www.ncei.noaa.gov/products/optimum-interpolation-sst}
"sst_NW_Atl"
