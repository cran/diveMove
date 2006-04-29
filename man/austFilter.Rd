\name{austFilter}
\alias{austFilter}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Filter satellite locations}


\description{Apply a three stage algorithm to eliminate erroneous
  locations, based on the procedure outlined in Austin et al. (2003).}


\usage{
austFilter(time, lon, lat, id=gl(1, 1, length(time)),
           velthres, distthres, window=5)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{chron object with dates and times for each point.}

  \item{lon}{numeric vectors of longitudes, in decimal degrees.}

  \item{lat}{numeric vector of latitudes, in decimal degrees.}

  \item{id}{a factor grouping points in different categories
    (e.g. individuals).}

  \item{velthres}{velocity threshold above which filter tests should
    fail any given point.}

  \item{distthres}{distance threshold above which the last filter test
    should fail any given point.}

  \item{window}{integer indicating the size of the moving window over
    which tests should be carried out.}

}


\details{The first stage of the filter is an iterative process which
  tests every point, except the first and last two, for travel velocity
  relative to the preceeding/following two points. If all these four
  velocities are greater than the specified threshold, the point is
  marked as failing the first stage. In this case, the next point is
  tested, removing the failing point from the set of test points.

  The second stage runs McConnell et al. (1992) algorithm, which tests
  all the points that passed the first stage, in the same manner as
  above. The root mean square of all four velocities is calculated, and
  if it is greater than the specified threshold, the point is marked as
  failing the second stage.

  The third stage is run simultaneously with the second stage, but if
  the mean distance of all four pairs of points is greater than the
  specified threshold, then the point is marked as failing the third
  stage.}


\section{Warning}{This function applies McConnell et al.'s filter as
described in Austin et al. (2003), but other authors may have used the
filter differently. Austin et al. (2003) have apparently applied the
filter in a vectorized manner. It is not clear from the original paper
whether the filter is applied iteratively or in a vectorized fashion, so
authors may be using it inconsistently.}


\value{
  A matrix with three columns of logical vectors with values TRUE
  for points that failed each stage.
}


\note{Points that fail the first stage also fail the second and third
  stage, but points that fail the second stage do not necessarily fail
  the third stage.
}


\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\seealso{\code{\link{distSpeed}}}


\references{
  McConnell BJ, Chambers C, Fedak MA. 1992. Foraging ecology of southern
  elephant seals in relation to bathymetry and productivity of the
  Southern Ocean. \emph{Antarctic Science} 4:393-398.

  Austin D, McMillan JI, Bowen D. 2003. A three-stage algorithm for
  filtering erroneous Argos satellite locations. \emph{Marine Mammal
    Science} 19: 371-383.
}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{iteration}
