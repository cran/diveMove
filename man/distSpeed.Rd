\name{distSpeed}

\alias{distSpeed}

% -------------------------------------------------------------------------

\title{Calculate distance and speed between locations}



\description{Calculate distance, time difference, and speed between
  pairs of points defined by latitude and longitude, given the time at
  which all points were measured.}


\usage{
distSpeed(pt1, pt2, id=gl(1, nrow(pt1)))
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{pt1}{a matrix or data frame with three columns; the first a
    \code{POSIXct} object with dates and times for all points, the
    second and third numeric vectors of longitude and latitude for all
    points, respectively, in decimal degrees.}

  \item{pt2}{a matrix with the same structure as \code{pt1}.}

  \item{id}{a factor dividing the data in \code{txy} into distinct
    groups.}

}


\details{\code{pt1} and \code{pt2} may contain any number of rows.
  \code{track} is essentially a wrapper for \code{distSpeed}, taking a
  data frame, assumed to be ordered chronologically, and calculations
  are done between all successive rows.}


\value{A matrix with three columns: distance (km), time difference (s),
  and speed (m/s).}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{

ringy <- readLocs(system.file(file.path("data", "ringy.csv"),
                              package="diveMove"), dateCol=1,
                  dtformat="\%Y-\%m-\%d \%H:\%M:\%S", classCol=2, lonCol=3,
                  latCol=4)

## Travel summary between successive standard locations
ringy.std <- subset(ringy, subset=class == "0" | class == "1" |
                    class == "2" | class == "3")
ringy.travel <- distSpeed(ringy[-nrow(ringy), 3:5], ringy[-1, 3:5])
## Particular quantiles from travel summaries
quantile(ringy.travel[, 4], 0.99, na.rm=TRUE) # speed
quantile(ringy.travel[, 2], 0.99, na.rm=TRUE) # distance

## Travel summary between any two sets of points
distSpeed(ringy[c(1, 5, 10), 3:5], ringy[c(25, 30, 35), 3:5])

}



\keyword{math}% at least one, from doc/KEYWORDS
\keyword{manip}
