\name{origin}
\alias{origin}
\alias{origin<-}
\title{
  Chron Origin
}
\description{
  Get and set chron origin.
}
\usage{
origin(x)
origin(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  a chron object
}
  \item{value}{
	  month, day, year vector
}
}
\details{
\code{origin} extracts the origin of its argument.  \code{origin<-} sets the
origin of its argument.
}
\value{
	The \code{origin} function returns a month, day, year vector.
}
\note{
The default origin for chron objects is January 1, 1970 and it is recommended
that that origin be used.  Some chron functions may not perform properly
with a non-default origin.
}

\seealso{\code{\link{chron}}}

\examples{
ch <- as.chron(Sys.Date())
origin(ch)
}
\keyword{chron}
