\name{days}
\alias{days}
\alias{months}
\alias{quarters}
\alias{weekdays}
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
weekdays(x, abb = TRUE)
months(x, abb = TRUE)
quarters(x, abb = TRUE)
years(x)
}
\arguments{
  \item{x}{
    a dates object.
  }
  \item{abb}{
    should abbreviated names be returned? Default is \code{TRUE}.
}}
\value{
  an ordered factor corresponding to days, weekdays, months, quarters,
  or years of \code{x} for the respective function.
}
\seealso{
  \code{\link{is.weekend}}, \code{\link{is.holiday}}
}
\examples{
dts <- dates("07/01/78") + trunc(50 * rnorm(30))
plot(weekdays(dts))
plot(months(dts))
}
% Converted by Sd2Rd version 0.2-a3.
\keyword{chron}
