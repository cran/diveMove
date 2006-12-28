\name{detPhase}

\alias{detPhase}
\alias{getAct}

% -------------------------------------------------------------------------

\title{Detect phases of activity from depth readings}



\description{Functions to identify sections of a TDR record displaying
  one of three possible activities: on-land, at-sea, and at-sea
  leisure.}


\usage{
detPhase(time, depth, dry.thr, wet.thr, \dots)
getAct(time, act, interval)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with date and time for all depths.}

  \item{depth}{numeric vector with depth readings.}

  \item{dry.thr}{land error threshold in seconds.  On-land phases
    shorter than this threshold will be considered as at-sea.}

  \item{wet.thr}{at-sea leisure threshold in seconds. At-sea phases
    shorter than this threshold will be considered as at-sea leisure.}

  \item{act}{A numeric vector indicating the activity for every element
    of \code{time}.}

  \item{interval, \dots}{sampling interval in seconds.}

}


\details{\code{detPhase} first creates a factor with value \sQuote{L}
  (on-land) for rows with NAs for \code{depth} and value \sQuote{W}
  (at-sea) otherwise. It subsequently calculates the duration of each of
  these phases of activity.  If the duration of an on-land phase
  (\sQuote{L}) is less than \code{dry.thr}, then the values in the
  factor for that phase are changed to \sQuote{W} (at-sea).  The
  duration of phases is then recalculated, and if the duration of a
  phase of at-sea activity is less than \code{wet.thr}, then the
  corresponding value for the factor is changed to \sQuote{Z} (at-sea
  leisure).  The durations of all phases are recalculated a third time
  to provide final phase durations.

  \code{getAct} takes a factor indicating different activity phases,
  their associated time, and the sampling interval to return a factor
  uniquely identifying each phase of activity, i.e. labelling them.  In
  addition, it returns the duration of each phase, and their beginning
  and end times.}


\value{A list with components; the first 4 are returned by
  \code{detPhase} and the rest by \code{getAct}:

  \item{phase.id}{numeric vector identifying each activity phase,
    starting from 1 for every input record.}

  \item{trip.act}{factor with levels \sQuote{L} indicating land,
    \sQuote{W} indicating at-sea, \sQuote{U} for underwater (above dive
    criterion), \sQuote{D} for diving, \sQuote{Z} for at-sea leisure
    animal activities.  Only \sQuote{L}, \sQuote{W}, and \sQuote{Z} are
    actually represented.}

  \item{trip.beg}{a \code{POSIXct} object as long as the number of
    unique activity phases identified, indicating the start times for
    each activity phase.}

  \item{trip.end}{a \code{POSIXct} object as long as the number of
    unique activity phases identified, indicating the end times for each
    activity phase.}

  \item{time.br}{a factor dividing the factor \code{act} in phases.}

  \item{time.peract}{duration of each phase defined by \code{time.br}.}

  \item{beg.time}{\code{POSIXct} object; beginning time for each phase.}

  \item{end.time}{\code{POSIXct} object; ending time for each phase.}}


\seealso{\code{\link{detDive}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\keyword{manip}% at least one, from doc/KEYWORDS
