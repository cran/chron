\name{day.of.week}
\alias{day.of.week}
\alias{julian}
\alias{leap.year}
\alias{month.day.year}
\title{
  Convert between Julian and Calendar Dates
}
\usage{
julian(m, d, y, origin = c(month = 1, day = 1, year = 1970))
month.day.year(jul, origin = c(month = 1, day = 1, year = 1970))
leap.year(y)
day.of.week(month, day, year)
}
\arguments{
  \item{m}{
    vector of month numbers.
  }
  \item{d}{
    vector of day numbers.
  }
  \item{y}{
    vector of years.
  }
  \item{jul}{
    vector of Julian Dates, i.e., number of days since \code{origin}.
  }
  \item{origin}{
    vector specifying the origin as month, day, and year.  If missing,
    it defaults to \code{options(chron.origin)} if this is non-null,
    otherwise \code{c(month = 1, day = 1, year = 1970)}. 
}}
\value{
  A vector of Julian dates (number of days since \code{origin}) when
  \code{julian()} is called, or a list with members \code{month},
  \code{day}, \code{year} corresponding to the input Julian dates if
  \code{month.day.year()} is called.  \code{leap.year()} returns a
  logical vector indicating whether the corresponding year is a leap
  year.
  \code{day.of.week()} returns a number between 0 and 6 to specify day
  of the week--0 refers to Sunday.

  These functions were taken from Becker, Chambers, and Wilks (1988),
  and were slightly modified to take \code{chron} and \code{dates}
  objects; some also take the extra argument \code{origin}.
  %The original functions are stored in the \code{library(example)}.
}
\seealso{
  \code{\link{chron}},
  \code{\link{dates}},
  \code{\link{times}}
}
\examples{
julian(1, 1, 1970)
# [1] 0
unlist(month.day.year(0))
# month   day  year 
#     1     1  1970 
}
\keyword{chron}
% Converted by Sd2Rd version 0.2-a3.
