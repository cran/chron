\name{days}
\alias{days}
\alias{months.default}
\alias{quarters.default}
\alias{weekdays.default}
\alias{years}
\title{
  Return Various Periods from a Chron or Dates Object
}
\description{
  Given a chron or dates object, extract the year, quarter, month, day
  (within the month) or weekday (days within the week) of the date it
  represents.
}
\usage{
days(x)
\method{weekdays}{default}(x, abbreviate = TRUE)
\method{months}{default}(x, abbreviate = TRUE)
\method{quarters}{default}(x, abbreviate = TRUE)
years(x)
}
\arguments{
  \item{x}{an object inheriting from class \code{"\link{dates}"}, or
    coercible to such via \code{\link{as.chron}}.}
  \item{abbreviate}{
    should abbreviated names be returned?  Default is \code{TRUE}.
  }
}
\value{
  an ordered factor corresponding to days, weekdays, months, quarters,
  or years of \code{x} for the respective function.
}
\details{
  Note that \code{months}, \code{quarters} and \code{weekdays} are
  generics defined in package \pkg{base} which also provides methods for
  objects of class \code{"\link{Date}"} as generated, e.g., by
  \code{\link{Sys.Date}}.  These methods return \link{character} rather
  than \link{factor} variables as the default methods in \pkg{chron} do.
  To take advantage of the latter, Date objects can be converted to
  dates objects using \code{\link{as.chron}}, see the examples.
}
\seealso{
  \code{\link{is.weekend}},
  \code{\link{is.holiday}}
}
\examples{
dts <- dates("07/01/78") + trunc(50 * rnorm(30))
plot(weekdays(dts))
plot(months(dts))

## The day in the current timezone as a Date object.
Dt <- Sys.Date()
## Using the months method for Date objects.
months(Dt)
## Using the months default method.
months(as.chron(Dt))
}
\keyword{chron}
