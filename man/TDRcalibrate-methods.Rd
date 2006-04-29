\name{TDRcalibrate-methods}
\docType{methods}

\alias{diveAct-methods}
\alias{dPhaseLab-methods}
\alias{grossAct-methods}
\alias{show-methods}
\alias{tdr-methods}
\alias{velCoef-methods}
\alias{attendance-methods}

\alias{diveAct}
\alias{dPhaseLab}
\alias{grossAct}
\alias{tdr}
\alias{velCoef}
\alias{attendance}

\alias{diveAct,TDRcalibrate,missing-method}
\alias{diveAct,TDRcalibrate,character-method}
\alias{dPhaseLab,TDRcalibrate,missing-method}
\alias{dPhaseLab,TDRcalibrate,numeric-method}
\alias{grossAct,TDRcalibrate,missing-method}
\alias{grossAct,TDRcalibrate,character-method}
\alias{show,TDRcalibrate-method}
\alias{tdr,TDRcalibrate-method}
\alias{velCoef,TDRcalibrate-method}
\alias{attendance,TDRcalibrate,logical-method}


\title{Methods for querying "TDRcalibrate" objects}


\description{These methods can be used to extract elements or generating
  new information from \dQuote{TDRcalibrate} objects.}


\usage{
diveAct(x, y)
dPhaseLab(x, diveNo)
grossAct(x, y)
tdr(x)
velCoef(x)
attendance(obj, ignoreZ)
}


\arguments{

  \item{x, obj}{a \code{TDRcalibrate} object.}

  \item{y}{a string indicating the element from x to extract.  In the
    case of diveAct: \dQuote{dive.id}, \dQuote{dive.activity}, or
    \dQuote{postdive.id}.  In the case of grossAct: \dQuote{phase.id},
    \dQuote{trip.act}, \dQuote{trip.beg}, or \dQuote{trip.end}.}

  \item{diveNo}{numeric vector indicating the dive number to extract for
    \code{dPhaseLab}.}

  \item{ignoreZ}{logical indicating whether or not trivial aquatic
    activities should be ignored when calculating attendance.}

}


\details{If argument y is missing, then the entire element is extracted
  from the \code{TDRcalibrate} object.

  \code{diveAct} extracts vectors identifying all readings to a
  particular dive or postdive number, or a factor identifying all
  readings to a particular activity.

  \code{dPhaseLab} extracts a factor identifying all readings to a
  particular dive phase.

  \code{grossAct} extracts elements that divide the data into major
  activities.

  \code{tdr} and \code{velCoef} extract the \code{TDR} object and the
  velocity calibration coefficients, respectively.

  \code{attendance} generates an attendance table for the record;
  i.e. the duration of each dry and wet phase.

}


\seealso{\code{\link{detDive}}, \code{\link{detPhase}}}


\keyword{methods}
