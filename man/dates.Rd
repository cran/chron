\name{dates}
\alias{dates}
\alias{times}
\alias{Math.dates}
\alias{Ops.dates}
\alias{Summary.dates}
\alias{[<-.dates}
\alias{all.equal.dates}
\alias{as.data.frame.dates}
\alias{c.dates}
\alias{floor.dates}
\alias{format.dates}
\alias{print.dates}
\alias{trunc.dates}
\alias{Math.times}
\alias{Ops.times}
\alias{Summary.times}
\alias{[.times}
\alias{[<-.times}
\alias{[[.times}
\alias{as.character.times}
\alias{as.data.frame.times}
\alias{axis.times}
\alias{c.times}
\alias{diff.times}
\alias{format.times}
\alias{format<-.times}
\alias{hist.times}
\alias{identify.times}
\alias{is.na.times}
\alias{lines.times}
\alias{mean.times}
\alias{plot.times}
\alias{points.times}
\alias{print.times}
\alias{quantile.times}
\alias{summary.times}
\title{
  Generate Dates and Times Components from Input
}
\description{
  Create objects which represent dates or times.
}
\usage{
dates(x, \dots)
times(x, \dots)
}
\arguments{
  \item{x}{a chron object, a character vector, or a numeric vector
    specifying time. If character, it must be in a format recognized by
    \code{chron()}.  If numeric, it specifies Julian dates, i.e., number
    of days since an origin.}
  \item{...}{parameters for \code{chron()}.}
}
\value{
  An object of class \code{dates} or \code{times}, depending of the
  function called.

  These functions return objects inheriting from dates and times,
  respectively.  They call \code{chron()} if \code{x} does not belong to
  any of the chronological classes.
}
\examples{
dts <- dates(c("02/27/92", "02/27/92", "01/14/92",
               "02/28/92", "02/01/92"))
dts
# [1] 02/27/92 02/27/92 01/14/92 02/28/92 02/01/92
class(dts)

x <- chron(dates = c("02/27/92", "02/27/92", "01/14/92", "02/28/92"),
           times = c("23:03:20", "22:29:56", "01:03:30", "18:21:03"))
dates(x)
# [1] 02/27/92 02/27/92 01/14/92 02/28/92
}
\seealso{
  \code{\link{chron}},
  \code{\link{times}},
  \code{\link{seq.dates}},
  \code{\link{cut.dates}}
}
\keyword{chron}
