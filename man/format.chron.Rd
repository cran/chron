\name{format.chron}
\alias{format.chron}
\title{Format a chron object}
\description{
  Format a chron object.
}
\usage{
\method{format}{chron}(x, format = att$format, origin. = att$origin,
       sep = " ",  simplify, enclosed = c("(", ")"), ...)
}
\arguments{
  \item{x}{
A chron object.
}
  \item{format}{
As in \code{\link{chron}} or a single character string with percent codes as
detailed in \code{\link[base]{strptime}}.
}
  \item{origin.}{
As in \code{\link{chron}}.
}
  \item{sep}{
A character string that separates the dates and times in the output.  Ignored
if percent codes are used in \code{format}.
}
  \item{simplify}{
As in \code{\link{format.dates}}. Ignored 
if percent codes are used in \code{format}.
}
  \item{enclosed}{
A character vector of length 2 containing the strings that begin and end
each output date/time.
Ignored if percent codes are used in \code{format}.
}
  \item{\dots}{
Not currently used.
}
}
\value{
A character vector.
}

\seealso{
\code{\link{chron}}.
\code{\link{format.dates}}.
\code{\link[base]{strptime}}.
}
\examples{
format(chron(0, 0), c("yy/m/d", "h:m:s"), sep = "T", enclose = c("", ""))
format(chron(0, 0), "\%Y-\%m-\%dT\%H:\%M:\%S") # same
}
\keyword{misc}
