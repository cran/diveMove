\name{austFilter}

\alias{grpSpeedFilter}
\alias{rmsDistFilter}
\alias{austFilter}

% -------------------------------------------------------------------------

\title{Filter satellite locations}



\description{Apply a three stage algorithm to eliminate erroneous
  locations, based on the procedure outlined in Austin et al. (2003).}


\usage{
austFilter(time, lon, lat, id=gl(1, 1, length(time)),
           speed.thr, dist.thr, window=5)
grpSpeedFilter(x, speed.thr, window=5)
rmsDistFilter(x, speed.thr, window=5, dist.thr)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with dates and times for each
    point.}

  \item{lon}{numeric vectors of longitudes, in decimal degrees.}

  \item{lat}{numeric vector of latitudes, in decimal degrees.}

  \item{id}{a factor grouping points in different categories
    (e.g. individuals).}

  \item{speed.thr}{speed threshold (m/s) above which filter tests
    should fail any given point.}

  \item{dist.thr}{distance threshold (km) above which the last filter
    test should fail any given point.}

  \item{window}{integer indicating the size of the moving window over
    which tests should be carried out.}

  \item{x}{3-column matrix with column 1: \code{POSIXct} vector; column
    2: numeric longitude vector; column 3: numeric latitude vector.}

}


\details{These functions implement the location filtering procedure
  outlined in Austin et al. (2003).  \code{grpSpeedFilter} and
  \code{rmsDistFilter} can be used to perform only the first stage or
  the second and third stages of the algorithm on their own,
  respectively.  Alternatively, the three filters can be run
  sequentially using \code{austFilter}.

  The first stage of the filter is an iterative process which tests
  every point, except the first and last two, for travel velocity
  relative to the preceeding/following two points. If all these four
  speeds are greater than the specified threshold, the point is marked
  as failing the first stage. In this case, the next point is tested,
  removing the failing point from the set of test points.

  The second stage runs McConnell et al. (1992) algorithm, which tests
  all the points that passed the first stage, in the same manner as
  above. The root mean square of all four speeds is calculated, and if
  it is greater than the specified threshold, the point is marked as
  failing the second stage.

  The third stage is run simultaneously with the second stage, but if
  the mean distance of all four pairs of points is greater than the
  specified threshold, then the point is marked as failing the third
  stage.

  The speed and distance threshold should be obtained separately (see
  \code{\link{distSpeed}}).}


\section{Warning}{This function applies McConnell et al.'s filter as
described in Austin et al. (2003), but other authors may have used the
filter differently. Austin et al. (2003) have apparently applied the
filter in a vectorized manner. It is not clear from the original paper
whether the filter is applied iteratively or in a vectorized fashion, so
authors may be using it inconsistently.}


\value{

  \code{grpSpeedFilter} returns a logical vector indicating those lines
  that passed the test.

  \code{rmsDistFilter} and \code{austFilter} return a matrix with 2 or 3
  columns, respectively, of logical vectors with values TRUE for points
  that passed each stage.  For the latter, lines that don't pass the
  first stage do not pass the other stages.  The second and third
  columns returned by \code{austFilter}, as well as those returned by
  \code{rmsDistFilter} are independent of one another; i.e. lines that
  don't pass stage 2 do not necessarily not pass stage 3.

}


\seealso{\code{\link{distSpeed}}}


\references{McConnell BJ, Chambers C, Fedak MA. 1992. Foraging ecology
  of southern elephant seals in relation to bathymetry and productivity
  of the Southern Ocean. \emph{Antarctic Science} 4:393-398.

  Austin D, McMillan JI, Bowen D. 2003. A three-stage algorithm for
  filtering erroneous Argos satellite locations. \emph{Marine Mammal
  Science} 19: 371-383.}

% -------------------------------------------------------------------------

\examples{
ringy <- readLocs(system.file(file.path("data", "ringy.csv"),
                              package="diveMove"), dateCol=1,
                  dtformat="\%Y-\%m-\%d \%H:\%M:\%S", classCol=2, lonCol=3,
                  latCol=4)
summary(ringy)

## Austin et al.'s group filter
grp <- grpSpeedFilter(ringy[, 3:5], speed.thr=0.9)

## McConnell et al.'s filter (root mean square test), and distance test
rms <- rmsDistFilter(ringy[, 3:5], speed.thr=0.9, dist.thr=100)

## All three tests in sequence (Austin et al. procedure)
austin <- with(ringy,
               austFilter(time, lon, lat, speed.thr=0.9, dist.thr=100))

}


\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{iteration}
