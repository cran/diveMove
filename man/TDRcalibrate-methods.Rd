\name{TDRcalibrate-methods}
\docType{methods}
\alias{TDRcalibrate-methods}

\alias{getDAct}
\alias{getDPhaseLab}
\alias{getGAct}
\alias{getTDR}
\alias{getSpeedCoef}

\alias{getDAct,TDRcalibrate,missing-method}
\alias{getDAct,TDRcalibrate,character-method}
\alias{getDPhaseLab,TDRcalibrate,missing-method}
\alias{getDPhaseLab,TDRcalibrate,numeric-method}
\alias{getGAct,TDRcalibrate,missing-method}
\alias{getGAct,TDRcalibrate,character-method}
\alias{show,TDRcalibrate-method}
\alias{getTDR,TDRcalibrate-method}
\alias{getSpeedCoef,TDRcalibrate-method}
\alias{plot,TDRcalibrate,missing-method}

% -------------------------------------------------------------------------
\title{Methods to Print and Extract Basic Information from
  "TDRcalibrate" Objects}


\description{
  Plot, print summaries and extract information from "TDRcalibrate"
  objects.
}

\usage{
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

  \item{x}{\code{"\linkS4class{TDRcalibrate}"} object.}

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


\section{Plotting and Printing Methods}{

    \item{show}{\code{signature(object = "TDRcalibrate")}: prints an
      informative summary of the data.}

    \item{plot}{\code{signature(x = "TDRcalibrate", y = "missing")}:
      plot the TDR object, labelling identified sections of it (see
      \code{Usage}).}

  }


\section{Extractor Methods}{
  \describe{

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

    \item{getGAct}{\code{signature(x = "TDRcalibrate", y = "missing")}:
      extracts elements that divide the data into major wet and dry
      activities.}

    \item{getGAct}{\code{signature(x = "TDRcalibrate", y =
	"character")}: as the method for missing \code{y}, but extracts
	particular elements.}

    \item{getTDR}{\code{signature(x = "TDRcalibrate")}: extracts the
      \code{TDR} object.}

    \item{getSpeedCoef}{\code{signature(x = "TDRcalibrate")}: extracts
      the speed calibration coefficients.}

  }

}


\value{ The extractor methods return an object of the same class as
  elements of the slot they extracted. }

% -------------------------------------------------------------------------
% \examples{
% ## TODO
% }

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
