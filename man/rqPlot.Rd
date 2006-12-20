\name{rqPlot}

\alias{rqPlot}

% -------------------------------------------------------------------------

\title{Plot of quantile regression for speed calibrations}



\description{Plot of quantile regression for assessing quality of speed
  calibrations}


\usage{
rqPlot(rddepth, speed, z, contours, rqFit, main="qtRegression",
       xlab="rate of depth change (m/s)", ylab="speed (m/s)",
       colramp=colorRampPalette(c("white", "darkblue")))
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{speed}{speed in m/s.}

  \item{rddepth}{numeric vector with rate of depth change.}

  \item{z}{a list with the bivariate kernel density estimates (1st
    component the x points of the mesh, 2nd the y points, and 3rd the
    matrix of densities).}

  \item{contours}{numeric vector indicating which contours to draw;
    passed to \code{\link{contour}}.}

  \item{rqFit}{object of class \dQuote{rq} representing a quantile
    regression fit of rate of depth change on mean speed.}

  \item{main}{string; title prefix to include in ouput plot.}

  \item{xlab, ylab}{axis labels.}

  \item{colramp}{function taking an integer n as an argument and
    returning n colors.}

}


\details{The dashed line in the plot represents a reference indicating a
  one to one relationship between speed and rate of depth change.  The
  other line represent the quantile regression fit.}


\seealso{\code{\link{diveStats}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{arith}
\keyword{hplot}
