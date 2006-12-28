\name{calibrateDepth}

\alias{calibrateDepth}
\alias{calibrateSpeed}

% -------------------------------------------------------------------------

\title{Calibrate and build a "TDRcalibrate" object}



\description{These functions create a "TDRcalibrate" object which is
  necessary to obtain dive summary statistics.}


\usage{
calibrateDepth(x, dry.thr=70, wet.thr=3610, dive.thr=4, offset,
               descent.crit.q=0.1, ascent.crit.q=0.1, wiggle.tol=0.8)
calibrateSpeed(x, tau=0.1, contour.level=0.1, z=0, bad=c(0, 0),
               main=slot(getTDR(x), "file"), coefs, plot=TRUE,
               postscript=FALSE, \ldots)
}


\arguments{

  \item{x}{an object of class \code{TDR} for
    \code{\link{calibrateDepth}} or an object of class
    \code{TDRcalibrate} for \code{\link{calibrateSpeed}}.}

  \item{dry.thr, wet.thr}{arguments to \code{\link{detPhase}}.}

  \item{dive.thr}{argument to \code{\link{detDive}}.}

  \item{offset}{argument to \code{\link{zoc}}.}

  \item{descent.crit.q}{critical quantile of rates of descent below
    which descent is deemed to have ended.}

  \item{ascent.crit.q}{critical quantile of rates of ascent above which
    ascent is deemed to have started.}

  \item{wiggle.tol}{Proportion of maximum depth above which wiggles
    should not be allowed to define the end of descent.  It's also the
    proportion of maximum depth below which wiggles should be considered
    part of bottom phase.}

  \item{tau}{quantile on which to regress speed on rate of depth change;
    passed to \code{\link[quantreg]{rq}}.}

  \item{contour.level}{the mesh obtained from the bivariate kernel
    density estimation corresponding to this contour will be used for
    the quantile regression to define the calibration line.}

  \item{z}{only changes in depth larger than this value will be used for
    calibration.}

  \item{bad}{length 2 numeric vector indicating that only rates of depth
    change and speed greater than the given value should be used for
    calibration, respectively.}

  \item{coefs}{known speed calibration coefficients from quantile
    regression as a vector of length 2 (intercept, slope).  If provided,
    these coefficients are used for calibrating speed, ignoring all
    other arguments, except \code{x}.}

  \item{main, \ldots}{arguments passed to \code{\link{rqPlot}}.}

  \item{plot}{logical indicating whether to plot the results.}

  \item{postscript}{logical indicating whether to produce postscript
    file output.}

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


\value{An object of class \code{\link{TDRcalibrate-class}} for
  \code{\link{calibrateDepth}} and an object of class
  \code{\link{TDRspeed-class}} for \code{\link{calibrateSpeed}}.}


\seealso{\code{\link{detPhase}}, \code{\link{detDive}},
  \code{\link{zoc}}, \code{\link{rqPlot}}, for the underlying
  functions.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
