\name{is.holiday}
\alias{is.holiday}
\alias{is.weekend}
\title{
  Find Weekends and Holidays in a Chron or Dates Object
}
\usage{
is.weekend(x)
is.holiday(x, holidays)
}
\arguments{
  \item{x}{
    an object inheriting from \code{dates}.
  }
  \item{holidays}{
    optional \code{chron} or \code{dates} object listing holidays.  If
    argument \code{holidays} is missing, either the value of the object
    \code{.Holidays} (if it exists) or \code{NULL} will be used.
}}
\value{
  a logical object indicating whether the corresponding date is a
  weekend in the case of \code{is.weekend()} or a holiday in the case of
  \code{is.holiday()}.
}
\seealso{
  days, weekdays, months, quarters, years
}
\examples{
dts <- dates("01/01/98") + trunc(365 * runif(50))
table(is.weekend(dts))

.Holidays
# New Year Memorial Indepen. Labor day Thanksgiving Christmas 
# 01/01/92 05/25/92 07/04/92  09/07/92     11/26/92  12/25/92
# NOTE:  Only these 6 holidays from 1992 are defined by default!
}
\seealso{
  \code{\link{chron}},
  \code{\link{dates}},
  \code{\link{cut.dates}},
  \code{\link{seq.dates}}
}
% Converted by Sd2Rd version 0.2-a3.
