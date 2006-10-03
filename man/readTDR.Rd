\name{readTDR}

\alias{readTDR}
\alias{createTDR}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with TDR data}



\description{Read a comma delimited (*.csv) file containing time-depth
  recorder (TDR) data from various TDR models.  Models supported are
  MK5, MK7, and MK8 Wildlife Computers instruments.  Return a \code{TDR}
  or \code{TDRspeed} object. \code{buildTDR} creates an object of one of
  these classes from other objects in the session.}


\usage{
readTDR(file, dateCol=1, timeCol=2, depthCol=3, speedCol=6,
        subsamp=5, dtformat="\%d/\%m/\%Y \%H:\%M:\%S", tz="GMT")
createTDR(time, depth, speed, dtime, file)
}


\arguments{

  \item{file}{A string indicating the path to the file to read.}

  \item{dateCol}{Column number containing dates, and optionally, times.}

  \item{timeCol}{Column number with times.}

  \item{depthCol}{Column number containing depth readings.}

  \item{speedCol}{Column number containing speed readings.}

  \item{subsamp}{Subsample rows in \code{file} with \code{subsamp}
    interval, in s.}

  \item{dtformat}{A string, specifying the format in which the date and
    time columns, when pasted together, should be interpreted (see
    \code{\link{strptime}}).}

  \item{tz}{A string indicating the time zone assumed for the date and
    time readings.}

  \item{time}{a \code{POSIXct} object with date and time readings for
    locations.}

  \item{depth}{numeric vector with depth readings.}

  \item{speed}{optional numeric vector with speed readings.}

  \item{dtime}{sampling interval used in seconds.}

}


\details{The file name must contain the adjacent letter \dQuote{mk}
  somewhere to be able to identify the TDR model.  If the number
  following these letters is an 8, then a column for speed readings is
  expected, in addition to depth.

  The file must have a header row identifying each field, and all rows
  must be complete (i.e. have the same number of fields).  Field names
  need not follow any convention.  However, depth and speed should
  preferably be given in m, and \eqn{m \cdot s^{-1}}{m/s} for further
  analyses.}


\value{An object of class \sQuote{TDR} or \sQuote{TDRspeed}.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{
readTDR(system.file(file.path("data", "sealMK8.csv"),
                    package="diveMove"))
}


\keyword{manip}% at least one, from doc/KEYWORDS
