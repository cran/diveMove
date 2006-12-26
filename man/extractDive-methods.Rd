\name{extractDive-methods}
\docType{methods}
\alias{extractDive-methods}

\alias{extractDive}
\alias{extractDive,TDR,numeric,numeric-method}
\alias{extractDive,TDRcalibrate,numeric,missing-method}

% -------------------------------------------------------------------------
\title{Extract Dives from "TDR" or "TDRcalibrate" Objects}


\description{

}


\usage{
\S4method{extractDive}{TDR,numeric,numeric}(obj, diveNo, id)
\S4method{extractDive}{TDRcalibrate,numeric,missing}(obj, diveNo)
}


\arguments{

  \item{obj}{\code{"\linkS4class{TDR}"} object.}
  \item{diveNo}{numeric vector or scalar with dive numbers to extract.}
  \item{id}{numeric vector of dive numbers from where \code{diveNo}
    should be chosen.}

}


\value{
An object of class \code{TDR} or \code{TDRspeed}.
}

% -------------------------------------------------------------------------
% \examples{
% ## TODO
% }

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
