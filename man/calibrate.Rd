\name{calibrateDepth}
\alias{calibrateDepth}

\alias{calibrateVel}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calibrate and build a "TDRcalibrate" object}

\description{These functions create a "TDRcalibrate" object which is
  necessary to obtain dive summary statistics.}

\usage{
calibrateDepth(x, landerr=65, seaerr=3605, divethres=4, offset)
calibrateVel(x, type="all", calType="pooled", bad=c(0, 0),
z=0, filename=slot(tdr(x), "file"), coefs, \ldots)
}

%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{an object of class \code{TDR} for \code{calibrateDepth},
    and an object of class \code{TDRcalibrate-class} for
    \code{calibrateVel}.}

  \item{landerr, seaerr}{arguments to \code{\link{detPhase}}.}

  \item{divethres}{argument to \code{\link{detDive}}.}

  \item{offset}{argument to \code{\link{zoc}}.}

  \item{type, calType, bad, z, filename}{further arguments for
    \code{\link{.getVelCalib}} and \code{\link{doVelCalib}}.}

  \item{coefs}{known velocity calibration coefficients from quantile
    regression as a vector of length 2 (intercept, slope).  If provided,
    these coefficients are used for calibrating velocity, ignoring all
    other arguments, except \code{x}.}

  \item{\ldots}{argument passed to \code{\link{doVelCalib}}.}

}


\details{These functions are really wrappers around functions that are
  usually called in sequence, so they provided an abbreviated method for
  running them together during analyses.  See the functions in the
  \sQuote{See Also} section for more details.

  \code{calibrateDepth} performs zero-offset correction of depth,
  wet/dry phase detection, and detection of dives, as well as proper
  labelling of the latter.

  \code{calibrateVel} calibrates velocity readings.

}


\value{An object of class \code{\link{TDRcalibrate-class}}}


\seealso{\code{\link{detPhase}}, \code{\link{detDive}},
  \code{\link{doVelCalib}}, \code{\link{zoc}}, for the underlying
  functions.}


}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
