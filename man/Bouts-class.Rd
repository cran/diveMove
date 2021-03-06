% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClass.R
\docType{class}
\name{Bouts-class}
\alias{Bouts-class}
\alias{Bouts}
\title{Class "Bouts" for representing Poisson mixtures for identification of
behavioural bouts}
\description{
Base class for storing key information for modelling and detecting
bouts in behavioural data.
}
\section{Slots}{

\describe{
\item{\code{x}}{Object of class \code{"numeric"}.  Data to be modelled.}

\item{\code{method}}{Object of class \code{"character"}.  A string indicating
the type of frequency to calculate from \code{x}: "standard" or
"seq.diff".  If "standard", frequencies are calculated directly
from \code{x}, and from the sequential differences in \code{x}
otherwise.}

\item{\code{lnfreq}}{Object of class \code{\link{data.frame}}.  Columns named
\var{lnfreq} (log frequencies) and \var{x} (mid points of histogram
bins).}
}}

\section{Objects from the class}{

Objects can be created most conveniently via the
\code{\link{boutfreqs}} function, which sets the \code{lnfreq} slot,
but can also be created via \code{new("Bouts")}.
}

\seealso{
\code{\link{boutfreqs}}
}
\author{
Sebastian P. Luque \email{spluque@gmail.com}
}
\keyword{classes}
