\name{detDive}

\alias{detDive}
\alias{labDive}
\alias{labDivePhase}

% -------------------------------------------------------------------------

\title{Detect dives from depth readings}



\description{Identify dives in TDR records based on a dive threshold.}


\usage{
detDive(zdepth, act, dive.thr=4, \dots)
labDive(act, string, interval)
labDivePhase(x, diveID, descent.crit.q, ascent.crit.q, wiggle.tol)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{zdepth}{vector of zero-offset corrected depths.}

  \item{act}{factor as long as \code{depth} coding activity, with levels
  specified as in \code{\link{detPhase}}.}

  \item{dive.thr}{threshold depth below which an underwater phase
    should be considered a dive.}

  \item{string}{a character belonging to a level of \code{act} to search
    for and label sequentially.}

  \item{interval, \dots}{the sampling interval in seconds.}

  \item{x}{a class \sQuote{TDR} object}

  \item{diveID}{numeric vector indexing each dive (non-dives should be
    0)}

  \item{descent.crit.q}{critical quantile of rates of descent below
    which descent is deemed to have ended.}

  \item{ascent.crit.q}{critical quantile of rates of ascent above which
    ascent is deemed to have started.}

  \item{wiggle.tol}{Proportion of maximum depth above which wiggles
    should not be allowed to define the end of descent.  It's also the
    proportion of maximum depth below which wiggles should be considered
    part of bottom phase.}

}


\details{\code{\emph{detDive}} detects a dive whenever the zero-offset
  corrected depth in an underwater phase is below the supplied dive
  threshold.  The adjustment is done only for phases of at-sea activity,
  completely ignoring phases with other activity.

  \code{\emph{labDive}} assigns a unique number to each dive along a
  vector of depths, and equally numbering the subsequent postdive
  interval.

  \code{\emph{labDivePhase}} labels each row identifying it with a
  portion of the dive.}


\value{A data frame with the following elements for \code{detDive}

  \item{dive.id}{numeric vector numbering each dive in the record.}

  \item{dive.activity}{factor with levels \sQuote{L}, \sQuote{W},
    \sQuote{U}, \sQuote{D}, and \sQuote{Z}, see \code{\link{detPhase}}.
    All levels may be represented.}

  \item{postdive.id}{numeric vector numbering each postdive interval
    with the same value as the preceding dive.}

  \code{labDive} returns a matrix with as many rows as its first two
  arguments with two columns: dive.id, and postdive.id, each one
  sequentially numbering each dive and postdive period.

  \code{labDivePhase} returns a factor with levels \dQuote{D},
  \dQuote{DB}, \dQuote{B}, \dQuote{BA}, \dQuote{A}, \dQuote{DA}, and
  \dQuote{X}, breaking the input into descent, descent/bottom, bottom,
  bottom/ascent, ascent, and non-dive, respectively.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{detPhase}}, \code{\link{zoc}}}


\keyword{manip}% at least one, from doc/KEYWORDS
