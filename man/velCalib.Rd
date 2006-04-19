\name{doVelCalib}
\alias{doVelCalib}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Calibration of TDR velocity}


\description{Calibrate velocity readings from a TDR, based on the
  principles outlined in Blackwell et al. (1999).}


\usage{
doVelCalib(rates, vel, calType="pooled", bad=c(0, 0), filename,
           postscript=FALSE, \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{rates}{two-element list corresponding to descent and ascent
    phases of dives, respectively.  Each element should be a 3-column
    \code{\link{matrix}} with dive id, rate of depth change, and mean
    velocity.}

  \item{vel}{numeric vector with uncalibrated velocities.}

  \item{calType}{string specifying the type of calibration to perform.
    It should be one of \dQuote{descent}, \dQuote{ascent}, or
    \dQuote{pooled}.}

  \item{bad}{vector of length 2 indicating values for rate of depth
    change and mean velocity, respectively, below which data should be
    excluded to build the calibration curve.}

  \item{filename}{string indicating name of file to use as base name for
    the output postscript file.}

  \item{postscript}{logical; whether output plot to eps.}

  \item{\ldots}{arguments passed to rqPlot; currently, colramp only is
    recognized.}

}


\details{Provide calibrated velocities in a TDR record, using the
  quantile regression of velocity on rate of depth change, based on
  principles outlined in Blackwell et al. (1999).  Choice of calibrating
  against pooled, or descentr ascent phases.

  The function takes the rates of depth change and velocity, for each
  phase of the dive separately or combined (based on the value of
  \code{calType}).  It subsequently fits a quantile regression through
  the second percentile of the distribution of velocity conditional on
  rate of depth change.  The calibrated velocity is \eqn{v_{c} =
  \frac{v_{u} - a}{b}}{v_c=(v_u - a)/b}, where \eqn{v_{c}}{v_c} is the
  calibrated velocity, \eqn{v_{u}}{v_u} is the uncalibrated velocity,
  and \eqn{a}{a} and \eqn{b}{b} are the intercept and slope of the
  quantile regression, respectively.}


\value{
  If \code{calType} is not \dQuote{none}, a list of two elements:

  \item{coefficients}{numeric vector of length two with the intercept
    and the slope of the quantile regression defining the calibration
    curve.}

  \item{corrVel}{numeric vector as long as \code{vel} with the
    calibrated velocities.}

  A plot (possibly via \code{\link{postscript}}, depending on the value
  of postscript argument) of the calibration lines for all possible
  cases: \dQuote{descent}, \dQuote{ascent}, and \dQuote{pooled}, is
  created as a side effect.

}


\references{Blackwell, S.  (1999) A method for calibrating swim-speed
  recorders.  Marine Mammal Science \bold{15(3)}: 894-905.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\seealso{\code{\link{TDRcalibrate-class}}, \code{\link{rqPlot}}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}
