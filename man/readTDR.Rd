\name{readTDR}

\alias{readTDR}
\alias{createTDR}
% -------------------------------------------------------------------------

\title{Read comma-delimited file with TDR data}


\description{Read a comma delimited (*.csv) file containing time-depth
  recorder (TDR) data from various TDR models.  Models supported are
  MK5, MK7, and MK8 Wildlife Computers instruments.  Return a \code{TDR}
  or \code{TDRvel} object. \code{buildTDR} creates an object of one of
  these classes from other objects in the session.}


\usage{
readTDR(file, dateCol=1, timeCol=2, depthCol=3, velCol=6,
        subsamp=5, dtformat=c("d/m/y", "h:m:s"))
createTDR(time, depth, vel, dtime, file)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{file}{A string indicating the path to the file to read.}

  \item{dateCol}{Column number containing dates, and optionally, times.}

  \item{timeCol}{Column number with times.}

  \item{depthCol}{Column number containing depth readings.}

  \item{velCol}{Column number containing velocity readings.}

  \item{subsamp}{Subsample rows in \code{file} with \code{subsamp}
    interval, in s.}

  \item{dtformat}{a character vector of length 2, specifying the format
    of the date and time columns, in that order.  See
    \code{\link[chron]{chron}} for valid format specifications.}

  \item{time}{a chron object}

  \item{depth}{numeric vector with depth readings}

  \item{vel}{optional numeric vector with velocity readings}

  \item{dtime}{sampling interval used in chron units (d)}

}


\details{The file name must contain the adjacent letter \dQuote{mk}
  somewhere to be able to identify the TDR model.  If the number
  following these letters is an 8, then a column for velocity readings
  is expected, in addition to depth.

  The file must have a header row identifying each field, and all rows
  must be complete (i.e. have the same number of fields).  Field names
  need not follow any convention.  However, depth and velocity should
  preferably be given in m, and \eqn{m \cdot s^{-1}}{m/s} for further
  analyses.}


\value{An object of class \sQuote{TDR} or \sQuote{TDRvel}.}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{
readTDR(system.file(file.path("data", "sealMK8.csv"),
                    package="diveMove"))
}


% -------------------------------------------------------------------------
\keyword{manip}% at least one, from doc/KEYWORDS
