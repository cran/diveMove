\name{doSpeedCalib}

\alias{doSpeedCalib}

% -------------------------------------------------------------------------

\title{Calibration of TDR speed}



\description{Calibrate speed readings from a TDR, based on the
  principles outlined in Blackwell et al. (1999).}


\usage{
doSpeedCalib(rates, speed, calType="pooled", bad=c(0, 0), filename,
             postscript=FALSE, \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{rates}{two-element list corresponding to descent and ascent
    phases of dives, respectively.  Each element should be a 3-column
    \code{\link{matrix}} with dive id, rate of depth change, and mean
    speed.}

  \item{speed}{numeric vector with uncalibrated speeds.}

  \item{calType}{string specifying the type of calibration to perform.
    It should be one of \dQuote{descent}, \dQuote{ascent}, or
    \dQuote{pooled}.}

  \item{bad}{vector of length 2 indicating values for rate of depth
    change and mean speed, respectively, below which data should be
    excluded to build the calibration curve.}

  \item{filename}{string indicating name of file to use as base name for
    the output postscript file.}

  \item{postscript}{logical; whether output plot to eps.}

  \item{\ldots}{arguments passed to rqPlot; currently, xlab, ylab,
    and colramp.}

}


\details{Provide calibrated speeds in a TDR record, using the quantile
  regression of speed on rate of depth change, based on principles
  outlined in Blackwell et al. (1999).  Choice of calibrating against
  pooled, or descentr ascent phases.

  The function takes the rates of depth change and speed, for each phase
  of the dive separately or combined (based on the value of
  \code{calType}).  It subsequently fits a quantile regression through
  the second percentile of the distribution of speed conditional on rate
  of depth change.  The calibrated speed is \eqn{s_{c} = \frac{s_{u} -
  a}{b}}{s_c=(s_u - a)/b}, where \eqn{s_{c}}{s_c} is the calibrated
  speed, \eqn{s_{u}}{s_u} is the uncalibrated speed, and \eqn{a}{a} and
  \eqn{b}{b} are the intercept and slope of the quantile regression,
  respectively.}


\value{If \code{calType} is not \dQuote{none}, a list of two elements:

  \item{coefficients}{numeric vector of length two with the intercept
    and the slope of the quantile regression defining the calibration
    curve.}

  \item{corrSpeed}{numeric vector as long as \code{speed} with the
    calibrated speeds.}

  A plot (possibly via \code{\link{postscript}}, depending on the value
  of postscript argument) of the calibration lines for all possible
  cases: \dQuote{descent}, \dQuote{ascent}, and \dQuote{pooled}, is
  created as a side effect.}


\references{Blackwell, S.  (1999) A method for calibrating swim-speed
  recorders.  Marine Mammal Science \bold{15(3)}: 894-905.}


\seealso{\code{\link{TDRcalibrate-class}}, \code{\link{rqPlot}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}
