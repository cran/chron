\name{chron}
\alias{chron}
\alias{as.data.frame.chron}
\alias{format.chron}
\alias{print.chron}
\title{
  Create a Chronological Object 
}
\usage{
chron(dates, times, format = c("m/d/y","h:m:s"), out.format, origin)
}
\arguments{
  \item{dates}{
    character or numeric vector specifying dates.
    If character, \code{dates} are assumed to be in one of the date
    formats below; if numeric, \code{dates} are assumed to be Julian
    dates, i.e., number of days since \code{origin}. 
  }
  \item{times}{
    optional character or numeric vector specifying times of day.
    If character, \code{times} are assumed to be in one of the time
    formats below; if numeric, \code{times} are assumed to be fractions
    of a day.
  }
  \item{format}{
    vector or list specifying the input format of the input.
    The format can be either strings specifying one of the recognized
    formats below or a list of user-supplied functions to convert dates
    from character into Julian dates and vice versa.

    The dates format can be any permutation of the characters "d", "m",
    or "y" delimited by a separator (possibly null), e.g., "m/d/y",
    "d-m-y", "ymd", are all valid; the format can also be permutations
    of the words "day" "month" and "year", which produces the month
    name, e.g., "month day year" produces "April 20 1992", "day mon
    year" produces "20 Apr 1992".

    The times format can be any permutation of "h", "m", and "s"
    separated by any one non-special character.  The default is "h:m:s".
  }
  \item{out.format}{
    vector or list specifying date and time format for printing and
    output. Default is same as \code{format}.
  }
  \item{origin}{
    a vector specifying the date with respect to which Julian dates
    are computed. Default is \code{c(month = 1, day = 1, year = 1970)};
    you may set the option \code{chron.origin} to specify your own
    default, e.g.,
    \code{options(chron.origin = c(month=1, day=1, year=1990))}. 
  }
}
\value{
  An object of class \code{times} if only \code{times} were specified,
  \code{dates} if only \code{dates}, or \code{chron} if both
  \code{dates = } and \code{times = } were supplied.
  All these inherit from class \code{times}.

  These objects represent dates and times of day, and allow the
  following arithmetic and summaries:  subtraction \code{d1-d2},
  constant addition \code{d1+constants}, all logical comparisons,
  summaries \code{min()}, \code{max()}, and \code{range()} (which drop
  NAs by default); constants specify days (fractions are converted to
  time-of-day, e.g., 2.5 represents 2 days and 12 hours).  Operations
  such as sorting, differencing, etc., are automatically handled.

  There are methods for \code{as.character()}, \code{as.numeric()},
  \code{cut()}, \code{is.na()}, \code{print()}, \code{summary()},
  \code{plot()}, \code{lines()}, \code{lag()}, and the usual subsetting
  functions \code{[}, \code{[<-}.
  The functions \code{days()}, \code{months()}, \code{quarters()},
  \code{years()}, \code{weeks()}, \code{weekdays()}, \code{hours()},
  \code{minutes()}, and \code{seconds()} take any \code{chron} object as
  input and extract the corresponding time interval. 
  \code{cut()} is used to create ordered factors from \code{chron}
  objects.  Chronological objects may be used with the modeling software.

  The current implementation of \code{chron} objects does not handle
  time zones nor daylight savings time.
}
\seealso{
  \code{\link{dates}},
  \code{\link{times}},
  \code{\link{julian}},
  \code{\link{cut.dates}},
  \code{\link{seq.dates}}.
}
\examples{
dts <- dates(c("02/27/92", "02/27/92", "01/14/92",
               "02/28/92", "02/01/92"))
dts
# [1] 02/27/92 02/27/92 01/14/92 02/28/92 02/01/92
tms <- times(c("23:03:20", "22:29:56", "01:03:30",
               "18:21:03", "16:56:26"))
tms
# [1] 23:03:20 22:29:56 01:03:30 18:21:03 16:56:26
x <- chron(dates = dts, times = tms)
x
# [1] (02/27/92 23:03:19) (02/27/92 22:29:56) (01/14/92 01:03:30)
# [4] (02/28/92 18:21:03) (02/01/92 16:56:26)

# We can add or subtract scalars (representing days) to dates or
# chron objects:
c(dts[1], dts[1] + 10)
# [1] 02/27/92 03/08/92
dts[1] - 31
# [1] 01/27/92

# We can substract dates which results in a times object that
# represents days between the operands:
dts[1] - dts[3]
# Time in days:
# [1] 44

# Logical comparisons work as expected:
dts[dts > "01/25/92"]
# [1] 02/27/92 02/27/92 02/28/92 02/01/92
dts > dts[3]
# [1]  TRUE  TRUE FALSE  TRUE  TRUE

# Summary operations which are sensible are permitted and work as
# expected:
range(dts)
# [1] 01/14/92 02/28/92
diff(x)
# Time in days:
# [1]  -0.02319444 -44.89335648  45.72052083 -27.05876157
sort(dts)[1:3]
# [1] 01/14/92 02/01/92 02/27/92
}
% Converted by Sd2Rd version 0.2-a3.
\keyword{chron}
