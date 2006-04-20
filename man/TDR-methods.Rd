\name{TDR-methods}
\docType{methods}

\alias{extractDive-methods}
\alias{depth-methods}
\alias{tdrTime-methods}
\alias{velocity-methods}
\alias{dtime-methods}
\alias{extractDive}
\alias{depth}
\alias{tdrTime}
\alias{velocity}
\alias{dtime}

\alias{as.data.frame,TDR-method}
\alias{velocity,TDRvel-method}
\alias{depth,TDR-method}
\alias{dtime,TDR-method}
\alias{extractDive,TDR,numeric,numeric-method}
\alias{extractDive,TDRcalibrate,numeric,missing-method}
\alias{plot,TDR,missing-method}
\alias{plot,TDRvel,missing-method}
\alias{show,TDR-method}
\alias{tdrTime,TDR-method}


\title{Methods for querying "TDR" class objects}


\description{Functions for extracting and visualizing data from the
  above mentioned class.}


\usage{
extractDive(obj, diveNo, id)
depth(x)
dtime(x)
velocity(x)
tdrTime(x)
}


\arguments{
  \item{obj, x}{a \dQuote{TDR} object or inheriting from it. For
    \code{extractDive}, it can also be a \dQuote{TDRcalibrate} object.}

  \item{diveNo}{a numeric vector of integers specifying the dive(s) to
    be extracted.}

  \item{id}{a numeric vector of integers specifying where matches for
    \code{diveNo} should be sought.  It can be missing when \code{obj}
    is a \code{TDRcalibrate} objects.}
}


\details{\code{extractDive} extracts all data from a particular dive
  number or set of dive numbers.

  \code{depth}, \code{dtime}, \code{velocity}, and \code{tdrTime}
  extract depth, sampling interval, velocity, and time, respectively.}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
