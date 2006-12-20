\name{TDR-class}
\docType{class}

\alias{TDR-class}
\alias{TDRspeed-class}

\alias{as.TDRspeed}
\alias{ccData<-}
\alias{depth<-}
\alias{speed<-}
\alias{getCCData}
\alias{getDepth}
\alias{getDtime}
\alias{getFileName}
\alias{getTime}
\alias{getSpeed}

\alias{[,TDR-method}
\alias{as.data.frame,TDR-method}
\alias{coerce,TDR,data.frame-method}
\alias{as.TDRspeed,TDR-method}
\alias{coerce,TDR,TDRspeed-method}
\alias{getCCData,TDR,missing-method}
\alias{getCCData,TDR,character-method}
\alias{ccData<-,TDR,data.frame-method}
\alias{getDepth,TDR-method}
\alias{getDtime,TDR-method}
\alias{depth<-,TDR,numeric-method}
\alias{extractDive,TDR,numeric,numeric-method}
\alias{getFileName,TDR-method}
\alias{plot,TDR,missing-method}
\alias{plot,TDRspeed,missing-method}
\alias{show,TDR-method}
\alias{getTime,TDR-method}
\alias{getSpeed,TDRspeed-method}
\alias{speed<-,TDRspeed,numeric-method}

% -------------------------------------------------------------------------

\title{Classes "TDR" and "TDRspeed" for representing TDR information}



\description{These classes store information gathered by time-depth
  recorders.}


\usage{
\S4method{extractDive}{TDR,numeric,numeric}(obj, diveNo, id)

\S4method{plot}{TDRspeed,missing}(x, concurVars=NULL, concurVarTitles, ...)

}


\arguments{

  \item{obj, x}{\code{"\linkS4class{TDR}"} and
  \code{"\linkS4class{TDR}"} objects, respectively.}

  \item{diveNo}{numeric vector or scalar with dive numbers to extract.}

  \item{id}{numeric vector of dive numbers from where \code{diveNo}
    should be chosen.}

  \item{concurVars, concurVarTitles, ...}{arguments passed to
    plotDive. In this TDRspeed method, \var{concurVars} is a matrix with
    variables to plot, in addition to speed.  \var{concurVarTitles} in
    this case is a character vector with axis labels for speed and the
    additional variables supplied in \var{concurVars}.}

}


\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("TDR", \dots)} and \code{new("TDRspeed", \dots)}.

  \code{TDR} objects contain concurrent time and depth readings, as well
  as a string indicating the file the data originates from, and a number
  indicating the sampling interval for these data.  \code{TDRspeed}
  extends \code{TDR} objects containing additional concurrent speed
  readings.}


\section{Slots}{
  In class \emph{TDR}:
  \describe{

    \item{\code{file}:}{Object of class \code{"character"}, string
      indicating the file where the data comes from.}

    \item{\code{dtime}:}{Object of class \code{"numeric"}, sampling
      interval in seconds.}

    \item{\code{time}:}{Object of class \code{"POSIXct"}, time stamp for
      every reading.}

    \item{\code{depth}:}{Object of class \code{"numeric"}, depth (m)
      readings.}

  }

  Class \emph{TDRspeed} must also satisfy the condition that a component
  of the concurrentData slot is named speed or velocity, containing the
  measured speed, a vector of class \code{"numeric"} containing speed
  measurements in (m/s) readings.

  }

}


\section{Methods}{
  \describe{

    \item{[}{\code{signature(x="TDR")}: Subset a TDR object; these
      objects can be subsetted on a single index \var{i}.}

    \item{as.data.frame}{\code{signature(x="TDR")}: Coerce object to
      data.frame.}

    \item{as.data.frame}{\code{signature(x="TDRspeed")}: Coerce object
      to data.frame.}

    \item{as.TDRspeed}{\code{signature(x="TDR")}: Coerce object to
      TDRspeed class.}

    \item{coerce}{\code{signature(from="TDR", to="data.frame")}: Coerce
      object to data.frame.}

    \item{coerce}{\code{signature(from="TDRspeed", to="data.frame")}:
      Coerce object to data.frame.}

    \item{coerce}{\code{signature(from="TDR", to="TDRspeed")}: Coerce
      object to TDRspeed class.}

    \item{getDepth}{\code{signature(x = "TDR")}: depth slot accessor.}

    \item{depth<-}{\code{signature(x="TDR")}: depth replacement.}

    \item{getCCData}{\code{signature(x="TDR", y="missing")}:
      concurrentData slot accessor.}

    \item{getCCData}{\code{signature(x="TDR", y="character")}: access
      component named \var{y} in \var{x}.}

    \item{getDtime}{\code{signature(x = "TDR")}: sampling interval
      accessor.}

    \item{extractDive}{\code{signature(obj = "TDR", diveNo = "numeric",
	id = "numeric")}: extract particular dives.}

    \item{getFileName}{\code{signature(x="TDR")}: source file name
      accessor.}

    \item{plot}{\code{signature(x = "TDR", y = "missing")}: interactive
      graphical display of the data, with zooming and panning
      capabilities.}

    \item{plot}{\code{signature(x = "TDRspeed", y = "missing")}: As the
      TDR method, but also plots the speed slot.}

    \item{show}{\code{signature(object = "TDR")}: print an informative
      summary of the data.}

    \item{getTime}{\code{signature(x = "TDR")}: time slot accessor.}

    \item{getSpeed}{\code{signature(x = "TDRspeed")}: speed accessor for
      \code{TDRspeed} objects.}

    \item{speed<-}{\code{signature(x="TDR")}: speed replacement.}

    }

  }
}


\details{Since the data to store in objects of these clases usually come
  from a file, the easiest way to construct such objects is with the
  function \code{readTDR} to retrieve all the necessary information.
  The methods listed above can thus be used to access all slots.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{readTDR}}, \code{\link{TDRcalibrate-class}}.}


\keyword{classes}
