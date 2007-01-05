\name{dives}
\docType{data}

\alias{dives}
\alias{divesTDR}
\alias{divesTDRcalibrate}

% -------------------------------------------------------------------------

\title{Sample TDR data from a fur seal}



\description{This data set is meant to show the typical organization of
  a TDR *.csv file, suitable as input for \code{\link{readTDR}}, or to
  construct a \code{TDR} object.  \code{divesTDR} and
  \code{divesTDRcalibrate} are example \code{TDR} and
  \code{TDRcalibrate} objects.}


\format{

  A comma separated value (csv) file with 69560 TDR readings with the
  following columns:

  \item{date}{date}

  \item{time}{time}

  \item{depth}{depth in m}

  \item{light}{light level}

  \item{temperature}{temperature in C}

  \item{speed}{speed in m/s}

  The data are also provided as a TDR object (*.RData format) for
  convenience.
}


\details{The data are a subset of an entire TDR record, so they are not
  meant to make valid inferences from this particular
  individual/deployment.

  \code{divesTDR} is a \code{\link{TDR}} object representation of the
  data in \code{dives}.

  \code{divesTDRcalibrate} is a \code{\link{TDRcalibrate}} object
  representing the data in dives, calibrated at default criteria (see
  \code{\link{calibrateDepth}}), and 3 m offset.  }


\seealso{\code{\link{readTDR}}}


\source{Sebastian P. Luque, Christophe Guinet, John P.Y. Arnould}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{datasets}
