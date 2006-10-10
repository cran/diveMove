\name{calibrateDepth}

\alias{calibrateDepth}
\alias{calibrateSpeed}

% -------------------------------------------------------------------------

\title{Calibrate and build a "TDRcalibrate" object}



\description{These functions create a "TDRcalibrate" object which is
  necessary to obtain dive summary statistics.}


\usage{
calibrateDepth(x, landerr=70, seaerr=3610, divethres=4, offset)
calibrateSpeed(x, type="all", calType="pooled", bad=c(0, 0),
               z=0, filename=slot(getTDR(x), "file"), coefs, \ldots)
}


\arguments{

  \item{x}{an object of class \code{TDR} for \code{calibrateDepth}, and
    an object of class \code{TDRcalibrate-class} for
    \code{calibrateSpeed}.}

  \item{landerr, seaerr}{arguments to \code{\link{detPhase}}.}

  \item{divethres}{argument to \code{\link{detDive}}.}

  \item{offset}{argument to \code{\link{zoc}}.}

  \item{type, calType, bad, z, filename}{further arguments for
    \code{\link{.getSpeedCalib}} and \code{\link{doSpeedCalib}}.}

  \item{coefs}{known speed calibration coefficients from quantile
    regression as a vector of length 2 (intercept, slope).  If provided,
    these coefficients are used for calibrating speed, ignoring all
    other arguments, except \code{x}.}

  \item{\ldots}{argument passed to \code{\link{doSpeedCalib}}.}

}


\details{These functions are really wrappers around functions that are
  usually called in sequence, so they provided an abbreviated method for
  running them together during analyses.  See the functions in the
  \sQuote{See Also} section for more details.

  \code{calibrateDepth} performs zero-offset correction of depth,
  wet/dry phase detection, and detection of dives, as well as proper
  labelling of the latter.

  \code{calibrateSpeed} calibrates speed readings.

}


\value{An object of class \code{\link{TDRcalibrate-class}}}


\seealso{\code{\link{detPhase}}, \code{\link{detDive}},
  \code{\link{doSpeedCalib}}, \code{\link{zoc}}, for the underlying
  functions.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
