\name{diveMove-package}
\docType{package}

\alias{diveMove-package}
\alias{diveMove}
\concept{time depth recorder}
\concept{Dive analysis}
% -------------------------------------------------------------------------

\title{Time depth recorder analysis}



\description{This package is a collection of functions for visualizing,
  and analyzing depth and speed data from time-depth recorders
  \dfn{TDR}s.  These can be used to zero-offset correct depth, calibrate
  speed, and divide the record into different phases, or time budget.
  Functions are provided for calculating summary dive statistics for the
  whole record, or at smaller scales within dives.}


\seealso{A vignette with a guide to this package is available by doing
  'vignette("diveMove")'.
  \code{\link[diveMove:diveMove-package]{TDR-class}},
  \code{\link[diveMove:diveMove-package]{calibrateDepth}},
  \code{\link[diveMove:diveMove-package]{calibrateSpeed}},
  \code{\link[diveMove:diveMove-package]{attendance}},
  \code{\link[diveMove:diveMove-package]{stampDive}}.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{
## read in data and create a TDR object
(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                              package="diveMove"), speed=TRUE))
\dontrun{
plot(sealX)  # pan and zoom through the record
}

## detect periods of activity, and calibrate depth, creating
## a 'TDRcalibrate' object
\dontrun{dcalib <- calibrateDepth(sealX)} # interactively
(dcalib <- calibrateDepth(sealX, offset=3)) # zero-offset correct at 3 m

\dontrun{
## plot all readings and label them with the phase of the record
## they belong to, excluding surface readings
plot(dcalib, labels="phase.id", surface=FALSE)
## plot the first 300 dives, showing dive phases and surface readings
plot(dcalib, diveNo=seq(300), labels="dive.phase", surface=TRUE)
}

## calibrate speed (using default remaining arguments)
(vcalib <- calibrateSpeed(dcalib))

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
