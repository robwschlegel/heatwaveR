% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exceedance.R
\name{exceedance}
\alias{exceedance}
\title{Detect consecutive days in exceedance above or below of a given threshold.}
\usage{
exceedance(
  data,
  x = t,
  y = temp,
  threshold,
  below = FALSE,
  minDuration = 5,
  joinAcrossGaps = TRUE,
  maxGap = 2,
  maxPadLength = FALSE,
  roundRes = 4,
  returnDF = TRUE
)
}
\arguments{
\item{data}{A data frame with at least the two following columns:
a \code{t} column which is a vector of dates of class \code{Date},
and a \code{temp} column, which is the temperature on those given
dates. If columns are named differently, their names can be supplied as \code{x}
and \code{y} (see below). The function will not accurately detect consecutive
days of temperatures in exceedance of the \code{threshold} if missing days of
data are not filled in with \code{NA}. Data of the appropriate format are created
by the internal function \code{\link{make_whole_fast}}, but your own data may be used
directly if they meet the given criteria. Note that it is also possible to provide hourly
data in the \code{x} column as class \code{POSIXct}.}

\item{x}{This column is expected to contain a vector of dates as per the
specification of \code{make_whole_fast}. If a column headed \code{t} is present in
the dataframe, this argument may be omitted; otherwise, specify the name of
the column with dates here. Note that it is also possible to provide hourly
data as class \code{POSIXct}.}

\item{y}{This is a column containing the measurement variable. If the column
name differs from the default (i.e. \code{temp}), specify the name here.}

\item{threshold}{The static threshold used to determine how many consecutive
days are in exceedance of the temperature of interest.}

\item{below}{Default is \code{FALSE}. When set to TRUE, consecutive days of temperature
below the \code{threshold} variable are calculated. When set to FALSE,
consecutive days above the \code{threshold} variable are calculated.}

\item{minDuration}{Minimum duration that temperatures must be in exceedance
of the \code{threshold} variable. The default is \code{5} days.}

\item{joinAcrossGaps}{A TRUE/FALSE statement that indicates whether
or not to join consecutive days of temperatures in exceedance of the
\code{threshold} across a small gap between groups before/after a short
gap as specified by \code{maxGap}. The default is \code{TRUE}.}

\item{maxGap}{The maximum length of the gap across which to connect
consecutive days in exceedance of the \code{threshold} when
\code{joinAcrossGaps = TRUE}.}

\item{maxPadLength}{Specifies the maximum length of days over which to
interpolate (pad) missing data (specified as \code{NA}) in the input
temperature time series; i.e., any consecutive blocks of NAs with length
greater than \code{maxPadLength} will be left as \code{NA}. Set as an
integer. The default is \code{3} days. Note this will be units of hours if
hourly data were provided.}

\item{roundRes}{This argument allows the user to choose how many decimal places
the exceedance metric outputs will be rounded to. Default is 4. To
prevent rounding set \code{roundRes = FALSE}. This argument may only be given
numeric values or FALSE.}

\item{returnDF}{The default (\code{TRUE}) tells the function to return the results as
type \code{data.frame}. \code{FALSE} will return the results as a \code{data.table}.}
}
\value{
The function will return a list of two data.frames.
The first being \code{threshold}, which shows the daily temperatures and on which
specific days the given \code{threshold} was exceeded. The second component of the
list is \code{exceedance}, which shows a medley of statistics for each discrete
group of days in exceedance of the given \code{threshold}. Note that any additional
columns left in the data frame given to this function will be output in the
\code{threshold} component of the output. For example, if one uses
\code{\link{ts2clm}} to prepare a time series for analysis and leaves
in the \code{doy} column, this column will appear in the output.

The information shown in the \code{threshold} component is:
  \item{t}{The date of the temperature measurement. This variable may named
  differently if an alternative name is supplied to the function's \code{x}
  argument.}
  \item{temp}{Temperature on the specified date [deg. C]. This variable may
  named differently if an alternative name is supplied to the function's \code{y}
  argument.}
  \item{thresh}{The static \code{threshold} chosen by the user [deg. C].}
  \item{thresh_criterion}{Boolean indicating if \code{temp} exceeds
  \code{threshold}.}
  \item{duration_criterion}{Boolean indicating whether periods of consecutive
  \code{thresh_criterion} are >= \code{minDuration}.}
  \item{exceedance}{Boolean indicting if all criteria that define a discrete
  group in exceedance of the \code{threshold} are met.}
  \item{exceedance_no}{A sequential number indicating the ID and order of
  occurrence of exceedances.}

The individual exceedances are summarised using the following metrics:
  \item{exceedance_no}{The same sequential number indicating the ID and
  order of the exceedance as found in the \code{threshold} component of the
  output list.}
  \item{index_start}{Row number on which exceedance starts.}
  \item{index_peak}{Row number on which exceedance peaks.}
  \item{index_end}{Row number on which exceedance ends.}
  \item{duration}{Duration of exceedance [days].}
  \item{date_start}{Start date of exceedance [date].}
  \item{date_peak}{Date of exceedance peak [date].}
  \item{date_end}{End date of exceedance [date].}
  \item{intensity_mean}{Mean intensity [deg. C].}
  \item{intensity_max}{Maximum (peak) intensity [deg. C].}
  \item{intensity_var}{Intensity standard deviation [deg. C].}
  \item{intensity_cumulative}{Cumulative intensity [deg. C x days].}
  \item{rate_onset}{Onset rate of exceedance [deg. C / day].}
  \item{rate_decline}{Decline rate of exceedance [deg. C / day].}

\code{intensity_max_abs}, \code{intensity_mean_abs}, \code{intensity_var_abs},
and \code{intensity_cum_abs} are as above except as absolute magnitudes rather
than relative to the threshold.
}
\description{
Detect consecutive days in exceedance above or below of a given threshold.
}
\details{
\enumerate{
\item This function assumes that the input time series consists of continuous
daily temperatures, with few missing values. The accompanying function
\code{\link{make_whole_fast}} aids in the preparation of a time series that is
suitable for use with \code{exceedance}, although this may also be accomplished
'by hand' as long as the criteria are met as discussed in the documentation
to \code{\link{make_whole_fast}}.
\item Future versions seek to accommodate monthly and annual time series, too.
\item The calculation of onset and decline rates assumes that exceedance of the
\code{threshold} started a half-day before the start day and ended a half-day
after the end-day. This is consistent with the duration definition as implemented,
which assumes duration = end day - start day + 1.
\item For the purposes of exceedance detection, any missing temperature values not
interpolated over (through optional \code{maxPadLength}) will remain as
\code{NA}. This means they will trigger the end of an exceedance if the adjacent
temperature values are in exceedance of the \code{threshold}.
\item If the function is used to detect consecutive days of temperature under
the given \code{theshold}, these temperatures are then taken as being in
exceedance below the \code{threshold} as there is no antonym in the English
language for 'exceedance'.
}
This function is based largely on the \code{detect_event} function found in this
package, which was ported from the Python algorithm that was written by Eric
Oliver, Institute for Marine and Antarctic Studies, University of Tasmania,
Feb 2015, and is documented by Hobday et al. (2016).
}
\examples{
res <- exceedance(sst_WA, threshold = 25)
# show first ten days of daily data:
res$threshold[1:10, ]
# show first five exceedances:
res$exceedance[1:5, ]

}
\author{
Robert W. Schlegel, Albertus J. Smit
}
