\name{diveMove-package}
\docType{package}

\alias{diveMove-package}
\alias{diveMove}
\concept{time depth recorder}
\concept{Dive analysis}
% -------------------------------------------------------------------------

\title{Time depth recorder analysis}



\description{This package is a collection of functions for visualizing,
  and analyzing depth and velocity data from time-depth recorders
  \dfn{TDR}s.  These can be used to zero-offset correct depth, calibrate
  velocity, and divide the record into different phases, or time budget.
  Functions are provided for calculating summary dive statistics for the
  whole record, or at smaller scales within dives.}


\seealso{A vignette with a guide to this package is available by doing
  'vignette("diveMove")'.
  \code{\link[diveMove:diveMove-package]{TDR-class}},
  \code{\link[diveMove:diveMove-package]{calibrateDepth}},
  \code{\link[diveMove:diveMove-package]{calibrateVel}},
  \code{\link[diveMove:diveMove-package]{attendance}},
  \code{\link[diveMove:diveMove-package]{stampDive}}.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{
## read in data and create a TDR object
(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                              package="diveMove")))
\dontrun{plot(sealX)}

## detect periods of activity, and calibrate depth, creating
## a 'TDRcalibrate' object
\dontrun{dcalib <- calibrateDepth(sealX)}
(dcalib <- calibrateDepth(sealX, offset=3)) # zero-offset correct at 3 m

## plot dive number 100
\dontrun{plot(extractDive(dcalib, 100))}
## plot dives all dives, showing dive phases
ddseq <- seq(max(getDAct(dcalib, "dive.id")))
dd <- extractDive(dcalib, ddseq)
\dontrun{plot(dd, phaseCol=getDPhaseLab(dcalib, ddseq))}

## calibrate velocity
(vcalib <- calibrateVel(dcalib))

## Obtain dive statistics for all dives detected
dives <- diveStats(vcalib)
head(dives)

## Attendance table
att <- attendance(vcalib, FALSE) # taking trivial aquatic activities into account
att <- attendance(vcalib, TRUE)  # ignoring them
## Add trip stamps to each dive
stamps <- stampDive(vcalib)
sumtab <- data.frame(stamps, dives)
head(sumtab)
}


\keyword{ package }
