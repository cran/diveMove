\name{TDR-methods}
\docType{methods}
\alias{TDR-methods}

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
\alias{getFileName,TDR-method}
\alias{plot,TDR,missing-method}
\alias{plot,TDRspeed,missing-method}
\alias{show,TDR-method}
\alias{getTime,TDR-method}
\alias{getSpeed,TDRspeed-method}
\alias{speed<-,TDRspeed,numeric-method}

% -------------------------------------------------------------------------
\title{Coerce, Extractor, and Replacement methods for class "TDR" objects}


\description{
Basic methods for manipulating objects of class "TDR".
}


\usage{
\S4method{plot}{TDRspeed,missing}(x, concurVars=NULL, concurVarTitles, ...)
}


\arguments{

  \item{x}{\code{"\linkS4class{TDR}"} object.}

  \item{concurVars, concurVarTitles, ...}{arguments passed to
    plotDive. In this TDRspeed method, \var{concurVars} is a matrix with
    variables to plot, in addition to speed.  \var{concurVarTitles} in
    this case is a character vector with axis labels for speed and the
    additional variables supplied in \var{concurVars}.}

}


\section{General Methods}{
  \describe{

    \item{plot}{\code{signature(x = "TDR", y = "missing")}: interactive
      graphical display of the data, with zooming and panning
      capabilities.}

    \item{plot}{\code{signature(x = "TDRspeed", y = "missing")}: As the
      TDR method, but also plots the speed slot.}

    \item{show}{\code{signature(object = "TDR")}: print an informative
      summary of the data.}

  }
}

\section{Coerce Methods}{
  \describe{

    \item{as.data.frame}{\code{signature(x="TDR")}: Coerce object to
      data.frame.}

    \item{as.data.frame}{\code{signature(x="TDRspeed")}: Coerce object
      to data.frame.}

    \item{as.TDRspeed}{\code{signature(x="TDR")}: Coerce object to
      TDRspeed class.}

  }
}

\section{Extractor Methods}{
  \describe{

    \item{[}{\code{signature(x="TDR")}: Subset a TDR object; these
      objects can be subsetted on a single index \var{i}.}

    \item{getDepth}{\code{signature(x = "TDR")}: depth slot accessor.}

    \item{getCCData}{\code{signature(x="TDR", y="missing")}:
      concurrentData slot accessor.}

    \item{getCCData}{\code{signature(x="TDR", y="character")}: access
      component named \var{y} in \var{x}.}

    \item{getDtime}{\code{signature(x = "TDR")}: sampling interval
      accessor.}

    \item{getFileName}{\code{signature(x="TDR")}: source file name
      accessor.}

    \item{getTime}{\code{signature(x = "TDR")}: time slot accessor.}

    \item{getSpeed}{\code{signature(x = "TDRspeed")}: speed accessor for
      \code{TDRspeed} objects.}

  }
}

\section{Replacement Methods}{
  \describe{

    \item{depth<-}{\code{signature(x="TDR")}: depth replacement.}

    \item{speed<-}{\code{signature(x="TDR")}: speed replacement.}

    \item{ccData<-}{\code{signature(x="TDR")}: concurrent data frame
      replacement.}

  }
}


\seealso{
\code{\link{extractDive}}, \code{\link{plotDive}}.
}

% -------------------------------------------------------------------------
% \examples{
% ## TODO
% }

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
