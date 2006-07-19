\name{rqPlot}

\alias{rqPlot}
% -------------------------------------------------------------------------

\title{Plot of quantile regression for velocity calibrations}


\description{Plot of quantile regression for assessing quality of
  velocity calibrations}


\usage{
rqPlot(rdepth, vel, rqFit, main="qtRegression",
       xlab="rate of depth change (m/s)",
       ylab="velocity (m/s)",
       colramp=colorRampPalette(c("white", "darkblue")))
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{vel}{velocity in m/s.}

  \item{rdepth}{numeric vector with rate of depth change.}

  \item{rqFit}{object of class \dQuote{rq} representing a quantile
    regression fit of rate of depth change on mean velocity.}

  \item{main}{string; title prefix to include in ouput plot.}

  \item{xlab, ylab}{axis labels.}

  \item{colramp}{function taking an integer n as an argument and
    returning n colors.}

}


\details{The dashed line in the plot represents a reference indicating a
  one to one relationship between velocity and rate of depth change.
  The other line represent the quantile regression fit.}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{doVelCalib}}, \code{\link{diveStats}}}


\examples{
\dontrun{
plot((sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                                   package="diveMove"))))
}
}


% -------------------------------------------------------------------------
\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{chron}
\keyword{arith}
\keyword{hplot}
