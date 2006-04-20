\name{diveMove-internal}

\alias{.createChron}
\alias{.cutDive}
\alias{.descAsc}
\alias{.getInterval}
\alias{.getVelCalib}
\alias{.getVelStats}
\alias{rmsDist}
\alias{stageOne}

\title{Internal diveMove Functions}


\description{Functions used for very particular tasks within larger
  functions in diveMove}


\usage{
.cutDive(x)
.createChron(date, time, dtformat)
.descAsc(x, phase, type=c("all", "strict"), interval, z=0)
.getInterval(time)
.getVelCalib(time, zdepth, vel, dives, phase, \dots)
.getVelStats(x, vdist)
rmsDist(x, velthres, window=5, distthres)
stageOne(x, velthres, window=5)
}


\arguments{

  \item{x}{a single dive's data; for \emph{\code{.cutDive}}: a 2-col
    matrix with subscript in original TDR object and non NA depths.  For
    \emph{\code{stageOne}} and \emph{\code{rmsDist}}, it's a matrix with
    cols: chron, lon and lat.  For \emph{.descAsc}: a 4-col matrix with
    dive id, time, depth, and velocity.  For \emph{.getVelStats}: a
    3-col matrix with time, depth, and velocity.}

  \item{date}{A vector to be converted to be converted to a chron
    object.}

  \item{time}{chron object representing time, or a vector to be
    converted to such an object (for \code{.createChron}).  It can be
    missing in the latter case.}

  \item{dtformat}{A vector of length 2 indicating the format in which
    \code{date} and \code{time} should be interpreted by \code{chron}.}

  \item{phase}{factor labelling each row for its phase in dive.}

  \item{type}{string indicating whether all points belonging to
    descent/ascent should be included (\dQuote{all}), or points shared
    with bottom phase should be excluded (\dQuote{srict}).}

  \item{interval}{sampling interval in chron units (d).}

  \item{z}{minimum depth differences to use.}

  \item{zdepth}{zero-offset corrected depth m.}

  \item{vel}{velocity in m/s.  For doVelCalib: uncalibrated velocities;
    ignored if \code{calType} is \dQuote{none}.}

  \item{dives}{3-col \code{data.frame} with dive id (numeric), activity
    (factor), and postdive id (numeric).}

  \item{\dots}{arguments to pass to .descAsc (\code{type},
    \code{interval}, and \code{z}).}

  \item{vdist}{vertical distance travelled during ascent or descent.}

  \item{velthres}{maximum velocity criteria for testing location
    validity.}

  \item{distthres}{maximum distance criteria for testing location
    validity.}
}



\details{These functions are not meant to be called directly by the
  user, as he/she could not care less (right?).  This may change in the
  future.

  \emph{.getVelCalib} extracts the rates of descent and ascent with
  associated mean velocity during descent and ascent phases,
  respectively and returns a list that is later manipulated by
  doVelCalib to calibrate velocity.  The velocity used for each rate of
  depth change corresponds to the velocity read for the last point,
  assuming that each velocity reading is the average velocity for the
  last measurement interval.}


\value{

  \emph{.getVelCalib}: A list with two elements (named \dQuote{descent}
  and \dQuote{ascent}).  Each element is a 2-column matrix with rate of
  depth change in the first column, and velocity in the second,
  corresponding to the descent phase of each dive.
}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}
