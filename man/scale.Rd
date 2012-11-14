\name{scale}
\alias{chron_trans}
\alias{scale_x_chron}
\alias{scale_y_chron}
\title{Coordinates and Positions for Chronological Objects}
\usage{
  chron_trans(format = "\%Y-\%m-\%d", n = 5)
  scale_x_chron(..., format = "\%Y-\%m-\%d", n = 5)
  scale_y_chron(..., format = "\%Y-\%m-\%d", n = 5)
}
\arguments{
  \item{format}{format string as described in \code{\link[base]{strptime}}.}
  \item{n}{Approximate number of axis ticks.}
  \item{...}{Passed to \code{\link[ggplot2]{scale_x_continuous}}.}
}
\description{
  \code{chron_trans} is a \pkg{ggplot2} transformer for \code{chron}.
  \code{scale_x_chron} and \code{scale_y_chron} are \pkg{ggplot2} scales.
}
\examples{
if(require("ggplot2")) {
    dd <- data.frame(tt = chron(1:10), value = 101:110)
    p <- ggplot(dd, aes(tt, value)) +
             geom_point() + scale_x_chron(format = "\%m-\%d")
    print(p)
}
}
