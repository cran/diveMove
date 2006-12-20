\name{diveStats}

\alias{diveStats}
\alias{oneDiveStats}
\alias{stampDive}

% -------------------------------------------------------------------------

\title{Per-dive statistics}



\description{Calculate dive statistics in TDR records.}


\usage{
diveStats(x)
oneDiveStats(x, interval, speed=FALSE)
stampDive(x, ignoreZ=TRUE)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{a \code{\link{TDRcalibrate-class}} object for
    \code{diveStats} and \code{stampDive}.  a data frame containing a
    single dive's data.}

  \item{interval}{sampling interval for interpreting \code{x}.}

  \item{speed}{logical; should speed statistics be calculated?}

  \item{ignoreZ}{logical indicating whether trips should be numbered
    considering all aquatic activities (\dQuote{W} and \dQuote{Z}) or
    ignoring \dQuote{Z} activities.}

}


\details{\code{diveStats} calculates various dive statistics based on
  time and depth for an entire TDR record.  \code{oneDiveStats} obtains
  these statistics from a single dive, and \code{stampDive} stamps each
  dive with associated trip information.}


\value{A \code{\link{data.frame}} with one row per dive detected
  (durations are in s, and linear variables in m):

  \item{begdesc}{A \code{POSIXct} object, specifying the start time of
    each dive.}

  \item{enddesc}{A \code{POSIXct} object, as \code{begdesc} indicating
    descent's end time.}

  \item{begasc}{A \code{POSIXct} object, as \code{begdesc} indicating
    the time ascent began.}

  \item{desctim}{descent duration of each dive.}

  \item{botttim}{bottom duration of each dive.}

  \item{asctim}{ascent duration of each dive.}

  \item{descdist}{numeric vector with descent depth.}

  \item{bottdist}{numeric vector with the sum of absolute depth
    differences while at the bottom of each dive; measure of amount of
    \dQuote{wiggling} while at bottom.}

  \item{ascdist}{numeric vector with ascent depth.}

  \item{desc.tdist}{numeric vector with descent total distance,
    estimated from speed.}

  \item{desc.mean.speed}{numeric vector with descent mean speed.}

  \item{desc.angle}{numeric vector with descent angle.}

  \item{bott.tdist}{numeric vector with bottom total distance, estimated
    from speed.}

  \item{bott.mean.speed}{numeric vector with bottom mean speed.}

  \item{asc.tdist}{numeric vector with ascent total distance, estimated
    from speed.}

  \item{asc.mean.speed}{numeric vector with ascent mean speed.}

  \item{asc.angle}{numeric vector with ascent angle.}

  \item{divetim}{dive duration.}

  \item{maxdep}{numeric vector with maximum depth.}

  \item{postdive.dur}{postdive duration.}

  \item{postdive.tdist}{numeric vector with postdive total distance,
    estimated from speed.}

  \item{postdive.mean.speed}{numeric vector with postdive mean speed.}

  The number of columns depends on the value of speed.

  \code{stampDive} returns a data.frame with trip number, trip type, and
  start and end times for each dive.}


\seealso{\code{\link{detPhase}}, \code{\link{zoc}},
  \code{\link{TDRcalibrate-class}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{arith}% at least one, from doc/KEYWORDS
\keyword{math}
