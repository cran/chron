\name{hours}
\alias{hours}
\alias{minutes}
\alias{seconds}
\title{
  Return Hours, Minutes, or Seconds from a Times Object
}
\usage{
hours(x)
minutes(x)
seconds(x)
}
\arguments{
\item{x}{
  an object inheriting from class "times".
}}
\value{
  the corresponding time period as an ordered factor.
}
\examples{
x <- chron(dates = c("02/27/92", "02/27/92", "01/14/92", "02/28/92"),
           times = c("23:03:20", "22:29:56", "01:03:30", "18:21:03"))
h <- hours(x)
y <- runif(4)
boxplot(y ~ h)
}
\seealso{
  \code{\link{chron}},
  \code{\link{dates}},
  \code{\link{times}}
}
% Converted by Sd2Rd version 0.2-a3.
