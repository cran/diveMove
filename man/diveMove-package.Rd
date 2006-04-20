\name{diveMove-package}
\alias{diveMove-package}

\alias{diveMove}
\docType{package}


\title{Time depth recorder analysis}


\description{This package is a collection of functions for visualizing,
  and analyzing depth and velocity data from time-depth recorders
  \dfn{TDR}s.  These can be used to zero-offset correct depth, calibrate
  velocity, and divide the record into different phases, or time budget.
  Functions are provided for calculating summary dive statistics for the
  whole record, or at smaller scales within dives.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\details{A vignette with a guide to this package is available at
  \url{file://../inst/doc/diveMove.pdf}}


\seealso{\code{\link[diveMove:diveMove-package]{TDR-class}},
  \code{\link[diveMove:diveMove-package]{calibrateDepth}},
  \code{\link[diveMove:diveMove-package]{calibrateVel}},
  \code{\link[diveMove:diveMove-package]{attendance}},
  \code{\link[diveMove:diveMove-package]{stampDive}}}


\examples{
## read in data and create a TDR object
(sealX <- readTDR(system.file(file.path("data", "/sealMK8.csv"),
                              package="diveMove")))
\dontrun{plot(sealX)}

## detect periods of activity, and calibrate depth, creating
## a 'TDRcalibrate' object
\dontrun{(dcalib <- calibrateDepth(sealX))}
(dcalib <- calibrateDepth(sealX, offset=3)) # zero-offset correct at 3 m

## plot dive number 100
\dontrun{plot(extractDive(dcalib, 100))
## plot dives 160 to the last one and show dive phases
plot(extractDive(dcalib, 160:max(diveAct(dcalib, "dive.id"))),
     phaseCol=dPhaseLab(dcalib, 160:max(diveAct(dcalib, "dive.id"))))}

## calibrate velocity
(vcalib <- calibrateVel(dcalib))

## Obtain dive statistics for all dives detected
dives <- diveStats(vcalib)
## See 'chron' for converting the time columns (1-3) to the desired
## output format
head(dives)

## Attendance table
att <- attendance(vcalib, TRUE)  # ignoring trivial aquatic activities
att <- attendance(vcalib, FALSE)  # taking them into account
## Add trip stamps to each dive
stamps <- stampDive(vcalib)
sumtab <- data.frame(stamps, dives)
head(sumtab)
}


\keyword{ package }
