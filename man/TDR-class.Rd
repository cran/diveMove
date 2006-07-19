\name{TDR-class}
\docType{class}

\alias{TDR-class}
\alias{TDRvel-class}
\alias{chron-class}

\alias{getDepth}
\alias{getDtime}
\alias{getFileName}
\alias{getTime}
\alias{getVeloc}

\alias{as.data.frame,TDR-method}
\alias{coerce,TDR,data.frame-method}
\alias{getDepth,TDR-method}
\alias{getDtime,TDR-method}
\alias{extractDive,TDR,numeric,numeric-method}
\alias{getFileName,TDR-method}
\alias{plot,TDR,missing-method}
\alias{plot,TDRvel,missing-method}
\alias{show,TDR-method}
\alias{getTime,TDR-method}
\alias{getVeloc,TDRvel-method}
% -------------------------------------------------------------------------

\title{Classes "TDR" and "TDRvel" for representing TDR information}


\description{These classes store information gathered by time-depth
  recorders.}


\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("TDR", \dots)} and \code{new("TDRvel", \dots)}.

  \code{TDR} objects contain concurrent time and depth readings, as well
  as a string indicating the file the data originates from, and a number
  indicating the sampling interval for these data.  \code{TDRvel}
  extends \code{TDR} objects containing additional concurrent velocity
  readings.}


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


\section{Methods}{
  \describe{

    \item{as.data.frame}{\code{signature(x="TDR")}: Coerce object to
      data.frame.}

    \item{as.data.frame}{\code{signature(x="TDRvel")}: Coerce object to
      data.frame.}

    \item{coerce}{\code{signature(from="TDR", to="data.frame")}: Coerce
      object to data.frame.}

    \item{coerce}{\code{signature(from="TDRvel", to="data.frame")}:
      Coerce object to data.frame.}

    \item{getDepth}{\code{signature(x = "TDR")}: depth slot accessor.}

    \item{getDtime}{\code{signature(x = "TDR")}: sampling interval
      accessor.}

    \item{extractDive}{\code{signature(obj = "TDR", diveNo = "numeric",
	id = "numeric")}: extract particular dives.}

    \item{getFileName}{\code{signature(x="TDR")}: source file name
      accessor.}

    \item{plot}{\code{signature(x = "TDR", y = "missing")}: interactive
      graphical display of the data, with zooming and panning
      capabilities.}

    \item{plot}{\code{signature(x = "TDRvel", y = "missing")}: As the
      TDR method, but also plots the velocity slot.}

    \item{show}{\code{signature(object = "TDR")}: print an informative
      summary of the data.}

    \item{getTime}{\code{signature(x = "TDR")}: time slot accessor.}

    \item{getVeloc}{\code{signature(x = "TDRvel")}: velocity accessor
	for \code{TDRvel} objects.}

    }

  }
}


\details{Since the data to store in objects of these clases usually come
  from a file, the easiest way to construct such objects is by using the
  function \code{readTDR} to retrieve all the necessary information.
  The methods listed above can thus be used to access all slots.}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{readTDR}}, \code{\link{TDRcalibrate-class}}.}


% -------------------------------------------------------------------------
\keyword{classes}
