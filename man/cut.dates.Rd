\name{cut.dates}
\alias{cut.dates}
\title{
  Create a Factor from a Chron or Dates Object
}
\description{
  Divide the range of a chron or dates object \code{x} into intervals
  and code the values in \code{x} according to which interval they fall.
}
\usage{
cut.dates(x, breaks, labels, start.on.monday = TRUE)
}
\arguments{
  \item{x}{chron or dates object (see \code{chron}), character dates
    such as \code{"10/04/91"} or Julian dates).}
  \item{breaks}{either a vector of break points (a \code{dates} vector,
    character dates such as \code{"10/04/91"} or Julian dates), a
    constant specifying number of equally spaced intervals extending
    from \code{min(x)-1} to \code{max(x)+1}, or one of the strings in
    \code{c("days", "weeks", "months", "year")} specifying a time
    period.}
  \item{labels}{character labels for intervals.}
  \item{start.on.monday}{should  weeks be assumed to start on Mondays?
    Default is \code{TRUE}.  Set to \code{FALSE} if weeks start on
    Sundays; for other days of the week specify the corresponding
    number: Sunday == 0, Monday == 1, Tuesday == 2, \dots{}, Saturday ==
    6.}
}
\value{an ordered factor whose levels represent the various time
  intervals.}
\seealso{
  \code{\link{seq.dates}}}
\examples{
# days from 07/01/92 thru 07/15/92 fell into 3 Monday-started weeks
cut(dates("07/01/92") + 0:14, "weeks")
# [1] week 1 week 1 week 1 week 1 week 1 week 2 week 2 week 2 
# [9] week 2 week 2 week 2 week 2 week 3 week 3 week 3

dts <- dates(c("02/27/92", "02/27/92", "01/14/92",
               "02/28/92", "02/01/92"))
cut(dts, "months")
# [1] Feb 92 Feb 92 Jan 92 Feb 92 Feb 92
boxplot(runif(5) ~ cut(dts, "months")) 
% Converted by Sd2Rd version 0.2-a3.
\keyword{chron}
