\name{yearmon}
\alias{as.chron.yearmon} 
\alias{as.chron.yearqtr} 
\alias{as.chron.ts} 
\title{Convert monthly or quarterly data to chron}
\description{
  These functions can be used to convert the times of \code{"ts"} series
  with \code{frequency} of 12 or 4 or objects of \code{"yearmon"} and
  \code{"yearqtr"} class, as defined in the \pkg{"zoo"} package, to
  \code{chron} dates.
}
\usage{
\method{as.chron}{yearmon}(x, frac = 0, holidays = FALSE, ...)
\method{as.chron}{yearqtr}(x, frac = 0, holidays = FALSE, ...)
\method{as.chron}{ts}(x, frac = 0, holidays = FALSE, ...)
}
\arguments{
  \item{x}{an object of class \code{"\link[zoo]{yearmon}"} or
    \code{"\link[zoo]{yearqtr}"} or \code{"\link[stats]{ts}"} objects,
    or a numeric vector interpreted \dQuote{in years} and fractions of
    years.}
  \item{frac}{Number between zero and one inclusive representing
    the fraction of the way through the month or quarter.}
  \item{holidays}{If \code{TRUE} or a vector of chron dates, indicated
    holidays and weekends are excluded so the return value will be a
    non-holiday weekday.}
  \item{\dots}{Other arguments passed to \code{\link{chron}}.}
}
\details{
  The \code{"yearmon"} and \code{"yearqtr"} classes are defined in
  package \pkg{zoo}.  If \code{holidays} is \code{TRUE} or a vector of 
  dates then the \code{is.holiday} function is used to determine whether
  days are holidays.

  The method for ts objects converts the times corresponding to
  \code{time(x)} to chron.  The ts series must have a frequency that is
  a divisor of 12.
}
\value{
  Returns a \code{chron} object.  
}
\seealso{
  \code{\link{is.holiday}},
  \code{\link{ts}}
}
\examples{
## Monthly time series data.
as.chron(AirPassengers)
as.chron(time(AirPassengers))
## convert to first day of the month that is not a weekend or holiday
as.chron(AirPassengers, frac = 0, holidays = TRUE)
## convert to last day of the month
as.chron(AirPassengers, frac = 1)
## convert to last day of the month that is not a weekend or holiday
as.chron(AirPassengers, frac = 1, holidays = TRUE)
## convert to last weekday of the month
as.chron(AirPassengers, frac = 1, holidays = c())

## Quarterly time series data.
as.chron(presidents)
as.chron(time(presidents))
}
\keyword{ts}
