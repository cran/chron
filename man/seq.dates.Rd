\name{seq.dates}
\alias{seq.dates}
\title{
  Generate Chron or Dates Sequences
}
\usage{
seq.dates(from, to, by = "days", length.)
}
\arguments{
  \item{from}{
    starting date; it can be a chron or dates objet, a character string,
    e.g., "05/23/91", or a Julian date.
  }
  \item{to}{
    ending date, like \code{from}.
  }
  \item{by}{
    either a numeric value or one of the valid strings \code{"days"},
    \code{"weeks"}, \code{"months"}, or \code{"years"}.
  }
  \item{length.}{
    optional number of elements in the sequence.
}}
\value{a sequence with values (\code{form}, \code{from + by},
  \code{from + 2*by}, \code{\dots{}}, \code{to}) of class
  \code{class(from)} and origin \code{origin(from)}. 
  Note that \code{from} must be less than or equal to the argument
  \code{to}.
}
\examples{
seq.dates("01/01/92", "12/31/92", by = "months")
# [1] 01/01/92 02/01/92 03/01/92 04/01/92 05/01/92 06/01/92
# [7] 07/01/92 08/01/92 09/01/92 10/01/92 11/01/92 12/01/92

end.of.the.month <- seq.dates("02/29/92", by = "month", length = 15)
end.of.the.month
#  [1] 02/29/92 03/31/92 04/30/92 05/31/92 06/30/92 07/31/92
#  [7] 08/31/92 09/30/92 10/31/92 11/30/92 12/31/92 01/31/93
# [13] 02/28/93 03/31/93 04/30/93
}
\seealso{
  \code{\link{chron}},
  \code{\link{dates}},
  \code{\link{cut.dates}}
}
% Converted by Sd2Rd version 0.2-a3.
