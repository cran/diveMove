\name{zoc}

\alias{zoc}
\alias{plotDive}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Interactive zero-offset correction of TDR data}


\description{Correct zero-offset in TDR records, with the aid of a
  graphical user interface (GUI), allowing for dynamic selection of
  offset and multiple time windows to perform the adjustment.}


\usage{
zoc(time, depth, offset)
plotDive(time, depth, vel=NULL, xlim=NULL, phaseCol=NULL)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{chron object with date and time.}

  \item{depth}{numeric vector with depth in m.}

  \item{offset}{known amount of meters for zero-offset correcting depth
    throughout the entire TDR record.}

  \item{vel}{numeric vector with velocity in m/s.}

  \item{xlim}{vector of length 2, with lower and upper limits of time to
    be plotted.}

  \item{phaseCol}{factor dividing rows into sections.}
}


\details{These functions are used primarily to correct, visually, drifts
  in the pressure transducer of TDR records. \code{zoc} calls
  \code{plotDive}, which plots depth and, optionally, velocity vs. time
  with the possibility zooming in and out on time, changing maximum
  depths displayed, and panning through time. The option to zero-offset
  correct sections of the record gathers x and y coordinates for two
  points, obtained by clicking on the plot region. The first point
  clicked indicates the offset and beginning time of section to correct,
  and the second one indicates the ending time of the section to
  correct.  Multiple sections of the record can be corrected in this
  manner, by panning through the time and repeating the procedure.  In
  case there's overlap between zero offset corrected windows, the last
  one prevails.

  Once the whole record has been zero-offset corrected, remaining points
  with depth values lower than zero, are turned into zeroes, as these
  are assumed to be values at the surface.
}


\value{ \code{zoc} returns a numeric vector, as long as \code{depth} of
  zero-offset corrected depths.

  \code{plotDive} returns a list with as many components as sections of
  the record that were zero-offset corrected, each consisting of two
  further lists with the same components as those returned by
  \code{\link{locator}}.}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\seealso{\code{\link{detDive}}}


\keyword{iplot}% at least one, from doc/KEYWORDS
