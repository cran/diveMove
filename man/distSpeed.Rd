\name{distSpeed}
\alias{distSpeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{track}


\title{Calculate distance and speed between locations}


\description{Calculate distance, time difference, and velocity between
  pairs of points defined by latitude and longitude, given the time at
  which all points were measured.}


\usage{
distSpeed(pt1, pt2, velocity=TRUE)
track(txy, id=gl(1, nrow(txy)), subset)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{pt1}{a matrix or data frame with three columns; the first a
    chron object with dates and times for all points, the second and
    third numeric vectors of longitude and latitude for all points,
    respectively, in decimal degrees.}

  \item{pt2}{a matrix with the same structure as \code{pt1}.}

  \item{velocity}{logical; should velocity between points be
    calculated?}

  \item{txy}{a data frame with a chron object in its first column, lon
    and lat in second and third column, respectively.}

  \item{id}{a factor dividing the data in \code{txy} into distinct
    groups.}

  \item{subset}{a logical expression indicating the rows to be analyzed,
    in terms of elements of \code{txy}.}
}


\details{\code{pt1} and \code{pt2} may contain any number of rows.
  \code{track} is essentially a wrapper for \code{distSpeed}, taking a
  data frame, assumed to be ordered chronologically, and calculations
  are done between all successive rows.}


\value{For \code{distSpeed}, a matrix with three columns: distance (km),
  time difference (h), and velocity (m/s).  For \code{track}, a data
  frame with an id column and the same columns as in \code{distSpeed}.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\keyword{math}% at least one, from doc/KEYWORDS
\keyword{manip}
