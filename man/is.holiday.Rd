\name{is.holiday}
\alias{is.holiday}
\alias{is.weekend}
\alias{.Holidays}
\title{
  Find Weekends and Holidays in a Chron or Dates Object
}
\description{
  Determine the date represented by a chron or dates object is a weekend
  or a holiday.
}
\usage{
is.weekend(x)
is.holiday(x, holidays)
.Holidays
}
\arguments{
  \item{x}{an object inheriting from \code{"dates"}.}
  \item{holidays}{optional \code{"chron"} or \code{"dates"} object
    listing holidays.  If argument \code{holidays} is missing, either
    the value of the object \code{.Holidays} (if it exists) or
    \code{NULL} will be used.}
}
\value{
  a logical object indicating whether the corresponding date is a
  weekend in the case of \code{is.weekend()} or a holiday in the case of
  \code{is.holiday()}.
}
\seealso{
  \code{\link{days}},
  \code{\link{weekdays}},
  \code{\link{months}},
  \code{\link{quarters}},
  \code{\link{years}};
  \code{\link{chron}},
  \code{\link{dates}},
  \code{\link{cut.dates}},
  \code{\link{seq.dates}}
}
\examples{
dts <- dates("01/01/98") + trunc(365 * runif(50))
table(is.weekend(dts))

.Holidays
# New Year Memorial Indepen. Labor day Thanksgiving Christmas 
# 01/01/92 05/25/92 07/04/92  09/07/92     11/26/92  12/25/92
# NOTE:  Only these 6 holidays from 1992 are defined by default!
}
% Converted by Sd2Rd version 0.2-a3.
\keyword{chron}
