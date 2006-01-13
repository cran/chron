\name{trunc.times}
\alias{trunc.times}
\title{Truncate Times Objects}
\description{
  Truncate times objects.
}
\usage{
\method{trunc}{times}(x, units = "days")
}
\arguments{
  \item{x}{a \code{"times"} object.}
  \item{units}{can be one of \code{"days"}, \code{"hours"},
    \code{"minutes"}, \code{"seconds"} or an unambiguous abbreviated
    version of any of those.}
}
\details{
  The time is truncated to the second, minute, hour or day or to the
  value specified.
}
\value{
  An object of class \code{"times"}.
}
\seealso{
  \code{\link{trunc}} for the generic function and default methods.
}

\examples{
tt <- times(c("12:13:14", "15:46:17"))
trunc(tt, "minutes")
trunc(tt, "min")
trunc(tt, times("00:01:00"))
trunc(tt, "00:01:00")
trunc(tt, 1/(24*60))

tt2 <- structure(c(3.0, 3.1, 3.5, 3.9), class = "times")
trunc(tt2, "day")
trunc(tt2)

}
\keyword{chron}
