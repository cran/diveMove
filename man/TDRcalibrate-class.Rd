\name{TDRcalibrate-class}
\docType{class}

\alias{TDRcalibrate-class}

\alias{attendance}
\alias{getDAct}
\alias{getDPhaseLab}
\alias{extractDive}
\alias{getGAct}
\alias{getTDR}
\alias{getSpeedCoef}

\alias{attendance,TDRcalibrate,logical-method}
\alias{getDAct,TDRcalibrate,missing-method}
\alias{getDAct,TDRcalibrate,character-method}
\alias{getDPhaseLab,TDRcalibrate,missing-method}
\alias{getDPhaseLab,TDRcalibrate,numeric-method}
\alias{extractDive,TDRcalibrate,numeric,missing-method}
\alias{getGAct,TDRcalibrate,missing-method}
\alias{getGAct,TDRcalibrate,character-method}
\alias{show,TDRcalibrate-method}
\alias{getTDR,TDRcalibrate-method}
\alias{getSpeedCoef,TDRcalibrate-method}
\alias{plot,TDRcalibrate,missing-method}

% -------------------------------------------------------------------------

\title{Class "TDRcalibrate" for dive analysis}



\description{This class holds information produced at various stages of
  dive analysis.  Methods are provided for extracting data from each
  slot.}


\usage{
\S4method{attendance}{TDRcalibrate,logical}(obj, ignoreZ)
\S4method{extractDive}{TDRcalibrate,numeric,missing}(obj, diveNo)
\S4method{getDAct}{TDRcalibrate,missing}(x)
\S4method{getDAct}{TDRcalibrate,character}(x, y)
\S4method{getDPhaseLab}{TDRcalibrate,missing}(x)
\S4method{getDPhaseLab}{TDRcalibrate,numeric}(x, diveNo)
\S4method{getGAct}{TDRcalibrate,missing}(x)
\S4method{getGAct}{TDRcalibrate,character}(x, y)
\S4method{plot}{TDRcalibrate,missing}(x, diveNo=seq(unique(getDAct(x, "dive.id"))),
     labels="phase.id", concurVars=NULL, surface=FALSE, ...)
}


\arguments{

  \item{x, obj}{\code{"\linkS4class{TDRcalibrate}"} object.}

  \item{ignoreZ}{logical indicating whether to ignore trivial aquatic
    periods.}

  \item{diveNo}{numeric vector with dive numbers to plot.}

  \item{labels}{one of \dQuote{phase.id} or \dQuote{dive.phase},
    specifying whether to label observations based on the gross phase ID
    of the \code{"\linkS4class{TDR}"} object, or based on each dive
    phase, respectively.}

  \item{concurVars}{character vector indicating which additional
    components from the concurrent data frame should also be plotted, if
    any.}

  \item{surface}{logical indicating whether to plot surface readings.}

  \item{...}{further arguments to \code{\link{plotDive}}.}

  \item{y}{string; \dQuote{dive.id}, \dQuote{dive.activity}, or
    \dQuote{postdive.id} in the case of \code{getDAct}, to extract the
    numeric dive ID, the factor identifying dive phases in each dive, or
    the numeric postdive ID, respectively.  In the case of
    \code{getGAct} it should be one of \dQuote{phase.id},
    \dQuote{trip.act}, \dQuote{trip.beg}, or \dQuote{trip.end}, to
    extract the numeric phase ID for each observation, a factor
    indicating what major activity the observation corresponds to, or
    the beginning and end times of each phase in the record, respectively.}
}


\section{Objects from the Class}{ Objects can be created by calls of the
  form \code{new("TDRcalibrate", \dots)}.  The objects of this class
  contain information necessary to divide the record into sections
  (e.g. land/water), dive/surface, and different sections within dives.
  They also contain the parameters used to calibrate speed and criteria
  to divide the record into phases.}


\section{Slots}{
  \describe{

    \item{\code{tdr}:}{Object of class \code{"TDR"}, with concurrent
      time, depth, and possibly speed (if \code{"TDRspeed"}).  See
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

    \item{\code{speed.calib.coefs}:}{Object of class \code{"numeric"}
      the intercept and slope derived from the speed calibration
      procedure.}

  }
}


\section{Methods}{
  \describe{

    \item{attendance}{\code{signature(obj = "TDRcalibrate", ignoreZ =
	"logical")}: generates an attendance table for the TDR record;
	the duration of each dry and wet phase.}

    \item{getDAct}{\code{signature(x = "TDRcalibrate", y = "missing")}:
      extracts a data frame with vectors identifying all readings to a
      particular dive and postdive number, and a factor identifying all
      readings to a particular activity.}

    \item{getDAct}{\code{signature(x = "TDRcalibrate", y =
	"character")}: as the method for missing \code{y}, but selects a
	particular vector to extract.}

    \item{getDPhaseLab}{\code{signature(x = "TDRcalibrate", diveNo =
	"missing")}: extracts a factor identifying all readings to a
	particular dive phase.}

    \item{getDPhaseLab}{\code{signature(x = "TDRcalibrate", diveNo =
	"numeric")}: as the method for missing \code{y}, but selects
	data from a particular dive number to extract.}

    \item{extractDive}{\code{signature(obj = "TDRcalibrate", diveNo =
	"numeric", id = "missing")}: extract particular dives.}

    \item{getGAct}{\code{signature(x = "TDRcalibrate", y = "missing")}:
      extracts elements that divide the data into major wet and dry
      activities.}

    \item{getGAct}{\code{signature(x = "TDRcalibrate", y =
	"character")}: as the method for missing \code{y}, but extracts
	particular elements.}

    \item{show}{\code{signature(object = "TDRcalibrate")}: prints
      informative summary of the data.}

    \item{getTDR}{\code{signature(x = "TDRcalibrate")}: extracts the
      \code{TDR} object.}

    \item{getSpeedCoef}{\code{signature(x = "TDRcalibrate")}: extracts
      the speed calibration coefficients.}

    \item{plot}{\code{signature(x = "TDRcalibrate", y = "missing")}:
      plot the TDR object, labelling identified sections of it (see
      \code{Usage} and \code{Details}).}

  }

}


\details{This is perhaps the most important class in diveMove, as it
  holds all the information necessary for calculating requested
  summaries for a TDR.

  The \code{tdr} slot contains the time, zero-offset corrected depth,
  and possibly calibrated or uncalibrated speed.  See
  \code{\link{readTDR}} and the accessor function \code{\link{getTDR}}
  for this slot.  Convenient access to each vector in this slot is
  available through \code{\link{getTime}}, \link{getDepth}, and
  \link{getSpeed}.

  The slot gross.activity holds, as a list, a vector (named
  \code{phase.id}) numbering each major activity phase found in the
  record, a factor (named \code{trip.act}) labelling each row as being
  on-land, at-sea, or leisure at-sea activity.  These two elements are
  as long as there are rows in \code{tdr}.  This slot also contains two
  more vectors: one with the beginning time of each phase, and another
  with the ending time; both represented as \code{POSIXct} objects.  See
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
  \code{\link{getDAct}} to access all or any one of these vectors.

  \code{dive.phases} is a slot corresponding to a factor that labels
  each row in the record as belonging to a particular phase of a dive.
  See \code{\link{labDivePhase}}, and \code{\link{getDPhaseLab}} to
  access this slot.

  \code{land.threshold}, \code{sea.threshold}, \code{dive.threshold},
  and \code{speed.calib.coefs} are each a single number representing
  parameters used for detecting phases, and calibrating the TDR.  Except
  for the latter, these are mostly for internal use, and hence do not
  have an accessor function.  See \code{\link{getSpeedCoef}} for
  accessing \code{speed.calib.coefs}.

  The plot method for this class creates an interactive plot of the
  \code{"\linkS4class{TDR}"} object, labelling the identified
  observations with a chosen factor from the corresponding
  \code{"\linkS4class{TDRcalibrate}"} object.  The argument
  \code{surface} allows for the inclusion/exclusion of identified
  surface readings.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{TDR-class}} for links to other classes in the
  package}


\keyword{classes}
