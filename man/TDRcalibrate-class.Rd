\name{TDRcalibrate-class}
\docType{class}

\alias{TDRcalibrate-class}

\title{Class "TDRcalibrate" for dive analysis}

\description{This class holds information produced at various stages of
  dive analysis.  Methods are provided for extracting data from each
  slot.}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("TDRcalibrate", \dots)}.
  The objects of this class contain information necessary to divide the
  record into sections (e.g. land/water), dive/surface, and different
  sections within dives.  They also contain the parameters used to
  calibrate velocity and criteria to divide the record into phases.
}

\section{Slots}{
  \describe{

    \item{\code{tdr}:}{Object of class \code{"TDR"}, with concurrent time,
      depth, and possibly velocity (if \code{"TDRvel"}).  See
      \code{Details}.}

    \item{\code{gross.activity}:}{Object of class \code{"list"}, must be
      the same as value returned by \code{\link{detPhase}}.}

    \item{\code{dive.activity}:}{Object of class \code{"data.frame"},
      must be the same as value returned by \code{\link{detDive}}.}

    \item{\code{dive.phases}:}{Object of class \code{"factor"}, must be
      the same as value returned by \code{\link{labDivePhase}}.}

    \item{\code{land.threshold}:}{Object of class \code{"numeric"} the
      temporal criteria used for detecting periods on land that should
      be considered as at-sea.}

    \item{\code{sea.threshold}:}{Object of class \code{"numeric"} the
      temporal criteria used for detecting periods at-sea that should
      not be considered as foraging time.}

    \item{\code{dive.threshold}:}{Object of class \code{"numeric"} the
      criteria used for defining a dive.}

    \item{\code{vel.calib.coefs}:}{Object of class \code{"numeric"} the
      intercept and slope derived from the velocity calibration
      procedure.}

  }
}


\details{This is perhaps the most important class in diveMove, as it
  holds all the information necessary for calculating requested
  summaries for a TDR.

  The \code{tdr} slot contains the time, zero-offset corrected depth,
  and possibly calibrated or uncalibrated velocity.  See
  \code{\link{readTDR}} and the accessor function \code{\link{tdr}} for
  this slot.  Convenient access to each vector in this slot is available
  through \code{\link{tdrTime}}, \link{depth}, and \link{velocity}.

  The slot gross.activity holds, as a list, a vector (named
  \code{phase.id}) numbering each major activity phase found in the
  record, a factor (named \code{trip.act}) labelling each row as being
  on-land, at-sea, or leisure at-sea activity.  These two elements are
  as long as there are rows in \code{tdr}.  This slot also contains two
  more vectors: one with the beginning time of each phase, and another
  with the ending time; both represented as \code{chron} objects.  See
  \code{detPhase}.

  The slot \code{dive.activity} contains a data.frame, again with as
  many rows as those in \code{tdr}, consisting of three vectors named:
  \code{dive.id}, which is an integer vector, sequentially numbering
  each dive (rows that are not part of a dive are labelled 0),
  dive.activity is a factor which completes that in \code{trip.act}
  above, further identifying rows in the record belonging to a dive.
  The third vector in \code{dive.activity} is an integer vector
  sequentially numbering each postdive interval (all rows that belong to
  a dive are labelled 0).  See \code{\link{detDive}}, and
  \code{\link{diveAct}} to access all or any one of these vectors.

  \code{dive.phases} is a slot corresponding to a factor that labels
  each row in the record as belonging to a particular phase of a dive.
  See \code{\link{labDivePhase}}, and \code{\link{dPhaseLab}} to access
  this slot.

  \code{land.threshold}, \code{sea.threshold}, \code{dive.threshold},
  and \code{vel.calib.coefs} are each a single number representing
  parameters used for detecting phases, and calibrating the TDR.  Except
  for the latter, these are mostly for internal use, and hence do not
  have an accessor function.  See \code{\link{velCoef}} for accessing
  \code{vel.calib.coefs}.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\seealso{\code{\link{TDR-class}} for links to other classes in the
  package}


\keyword{classes}
