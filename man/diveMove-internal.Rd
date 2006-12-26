\name{diveMove-internal}

\alias{.cutDive}
% \alias{.descAsc}
\alias{.diveIndices}
\alias{.getInterval}
% \alias{.getSpeedCalib}
\alias{.getSpeedStats}

% -------------------------------------------------------------------------

\title{Internal diveMove Functions}



\description{Functions used for very particular tasks within larger
  functions in diveMove}


\usage{
.cutDive(x, descent.crit.q, ascent.crit.q, wiggle.tol)
% .descAsc(x, phase, type=c("all", "strict"), interval, z=0)
.diveIndices(diveID, diveNo)
.getInterval(time)
% .getSpeedCalib(time, zdepth, speed, dives, phase, \dots)
.getSpeedStats(x, vdist)
}


\arguments{

  \item{x}{a single dive's data; for \emph{\code{.cutDive}}: a 2-col
    matrix with subscript in original TDR object and non NA depths.  For
    \emph{.descAsc}: a 4-col matrix with dive id, time, depth, and
    speed.  For \emph{.getSpeedStats}: a 3-col matrix with time, depth,
    and speed.}

  \item{time}{\code{POSIXct} object representing time.}

  % \item{dtformat}{A string to interpret date and time (see
  %   \code{\link{strptime}}.}

  % \item{phase}{factor labelling each row for its phase in dive.}

  % \item{type}{string indicating whether all points belonging to
  %   descent/ascent should be included (\dQuote{all}), or points shared
  %   with bottom phase should be excluded (\dQuote{srict}).}

  % \item{interval}{sampling interval in seconds.}

  % \item{z}{minimum depth differences to use.}

  % \item{zdepth}{zero-offset corrected depth m.}

  % \item{speed}{speed in m/s.  For doSpeedCalib: uncalibrated speeds;
  %   ignored if \code{calType} is \dQuote{none}.}

  % \item{dives}{3-col \code{data.frame} with dive id (numeric), activity
  %   (factor), and postdive id (numeric).}

  % \item{\dots}{arguments to pass to .descAsc (\code{type},
  %   \code{interval}, and \code{z}).}

  % \item{vdist}{vertical distance travelled during ascent or descent.}

  \item{diveID}{Numeric vector of all dive and non dive IDs.}

  \item{diveNo}{Numeric vector of unique dive IDs to index in
    \code{diveID}.}

  \item{descent.crit.q}{critical quantile below which descent is deemed
    to have ended.}

  \item{ascent.crit.q}{critical quantile above which ascent is deemed to
    have started.}

  \item{wiggle.tol}{tolerance to wiggles.}
}


\details{These functions are not meant to be called directly by the
  user, as he/she could not care less (right?).  This may change in the
  future.

  \emph{.getSpeedCalib} extracts the rates of descent and ascent with
  associated mean speed during descent and ascent phases, respectively
  and returns a list that is later manipulated by doSpeedCalib to
  calibrate speed.  The speed used for each rate of depth change
  corresponds to the speed read for the last point, assuming that each
  speed reading is the average speed for the last measurement interval.}


\value{\emph{.getSpeedCalib}: A list with two elements (named
  \dQuote{descent} and \dQuote{ascent}).  Each element is a 2-column
  matrix with rate of depth change in the first column, and speed in the
  second, corresponding to the descent phase of each dive.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}
