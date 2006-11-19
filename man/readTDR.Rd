\name{readTDR}

\alias{readTDR}
\alias{createTDR}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with TDR data}



\description{Read a comma delimited (*.csv) file containing time-depth
  recorder (TDR) data from various TDR models.  Return a \code{TDR} or
  \code{TDRspeed} object. \code{createTDR} creates an object of one of
  these classes from other objects.}


\usage{
readTDR(file, dateCol=1, timeCol=2, depthCol=3, speed=FALSE,
        subsamp=5, concurrentCols=4:6,
        dtformat="\%d/\%m/\%Y \%H:\%M:\%S", tz="GMT")
createTDR(time, depth, concurrentData, speed=FALSE, dtime, file)
}


\arguments{

  \item{file}{a string indicating the path to the file to read.}

  \item{dateCol}{column number containing dates, and optionally, times.}

  \item{timeCol}{column number with times.}

  \item{depthCol}{column number containing depth readings.}

  \item{speed}{for \code{readTDR}: Logical indicating whether speed is
    included in one of the columns of concurrentCols.  For createTDR:
    optional numeric vector with speed readings.}

  \item{subsamp}{subsample rows in \code{file} with \code{subsamp}
    interval, in s.}

  \item{concurrentCols}{column numbers to include as concurrent data
    collected.}

  \item{dtformat}{a string, specifying the format in which the date and
    time columns, when pasted together, should be interpreted (see
    \code{\link{strptime}}).}

  \item{tz}{a string indicating the time zone assumed for the date and
    time readings.}

  \item{time}{a \code{POSIXct} object with date and time readings for
    locations.}

  \item{depth}{numeric vector with depth readings.}

  \item{concurrentData}{data frame with additional, concurrent data
    collected.}

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
                    package="diveMove"), speed=TRUE)
}


\keyword{manip}% at least one, from doc/KEYWORDS
