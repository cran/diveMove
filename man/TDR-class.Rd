\name{TDR-class}
\docType{class}

\alias{TDR-class}
\alias{TDRvel-class}
\alias{chron-class}
\alias{coerce,TDR,data.frame-method}


\title{Classes "TDR" and "TDRvel" for representing TDR information}

\description{These classes store information gathered by time-depth
  recorders.}

\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("TDR", \dots)} and \code{new("TDRvel", \dots)}.

  \code{TDR} objects contain concurrent time and depth readings, as
  well as a string indicating the file the data originates from, and a
  number indicating the sampling interval for these data.  \code{TDRvel}
  objects contain, in addition, concurrent velocity readings.}

\section{Slots}{
  In class \emph{TDR}:
  \describe{
    \item{\code{file}:}{Object of class \code{"character"}, string
      indicating the file where the data comes from.}
    \item{\code{dtime}:}{Object of class \code{"numeric"}, sampling
      interval in \code{\link[chron]{chron}} units (d).}
    \item{\code{time}:}{Object of class \code{"chron"}, time stamp for
      every reading.}
    \item{\code{depth}:}{Object of class \code{"numeric"}, depth (m)
      readings.}
  }
  Class \emph{TDRvel} adds:
  \describe{
    \item{\code{velocity}:}{Object of class \code{"numeric"} velocity
      (m/s) readings.}
  }
}


\details{Since the data to store in objects of these clases usually come
  from a file, the easiest way to construct such objects is by using the
  function \code{readTDR} to retrieve all the necessary information.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\seealso{\code{\link{readTDR}}, \code{\link{TDRcalibrate-class}}}


\keyword{classes}
