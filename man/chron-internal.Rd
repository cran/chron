\name{chron-internal}
\title{Internal chron objects}
%\alias{clock2frac}
%\alias{convert.chron}
%\alias{convert.dates}
%\alias{convert.times}
%\alias{count.events}
%\alias{count.fields.str}
%\alias{day.abb}
%\alias{day.name}
%\alias{format<-}
%\alias{frac2clock}
%\alias{julian2mine}
%\alias{mine2julian}
%\alias{month.length}
%\alias{my.axis}
%\alias{new.chron}
%\alias{parse.format}
%\alias{unpaste}
\alias{year.expand}
\alias{year.strict}
\description{
  Internal chron objects.
}
\usage{
%clock2frac(str)
%convert.chron(x, format = c(dates = "m/d/y", times = "h:m:s"), origin.,
%              sep = " ", enclose = c("(", ")"), \dots)
%convert.dates(dates. = NULL, format = "m/d/y", origin., length. = 0, 
%              \dots)
%convert.times(times = NULL, format = "h:m:s", length. = 0, \dots)
%count.events(x, by)
%count.fields.str(str, sep = "")
%day.abb
%day.name
%frac2clock(f)
%julian2mine(x)
%mine2julian(str)
%month.length
%my.axis(x, simplify = TRUE, \dots)
%new.chron(x, new.origin = c(1, 1, 1970), shift = julian(new.origin[1], 
%          new.origin[2], new.origin[3], c(0, 0, 0)))
%parse.format(format, year.abb = getOption("chron.year.abb"), \dots)
%unpaste(str, sep = "/", fnames = NULL, nfields = NULL,
%        first = c(1, 3, 5), width = 2)
year.expand(y, cut.off = 30, century = c(1900, 2000), \dots)
year.strict(\dots)
}
\details{
  These are not to be called by the user.
}
\keyword{utilities}
\keyword{internal}
