\name{readLocs}
\alias{readLocs}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Read comma-delimited file with location data}


\description{Read a comma delimited (*.csv) file with (at least) time,
  latitude, longitude readings.}


\usage{
readLocs(file, loc.idCol, idCol, dateCol, timeCol=NULL,
         dtformat=c("m/d/y", "h:m:s"), classCol, lonCol, latCol,
         alt.lonCol=NULL, alt.latCol=NULL)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{file}{A string indicating the name of the file to read. Provide
    the entire path if the file is not on the current directory.}

  \item{loc.idCol}{Column number containing location ID.}

  \item{idCol}{Column number containing an identifier for locations
    belonging to different groups.}

  \item{dateCol}{Column number containing dates, and, optionally, times.}

  \item{timeCol}{Column number containing times.}

  \item{dtformat}{length-2 numeric vector specifying the format (as in
    \code{chron}) in which date and time, respectively, should be read in
    \code{file}}

  \item{lonCol}{Column number containing longitude readings.}

  \item{latCol}{Column number containing latitude readings.}

  \item{classCol}{Column number containing the ARGOS rating for each
    location.}

  \item{alt.lonCol}{Column number containing alternative longitude
    readings.}

  \item{alt.latCol}{Column number containing alternative latitude
    readings.}
}


\details{The file must have a header row identifying each field, and all
  rows must be complete (i.e. have the same number of fields). Field
  names need not follow any convention.}


\value{A data frame.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\keyword{manip}
