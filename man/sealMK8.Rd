\name{sealMK8}
\docType{data}

\alias{sealMK8}


\title{Sample TDR data from a fur seal}


\description{This data set is meant to show the organization a TDR *.csv
  file must have in order to be used as input for
  \code{\link{readTDR}}.}


\format{

  A comma separated value (csv) file with 69560 TDR readings with the
  following columns:

  \item{date}{date}

  \item{time}{time}

  \item{depth}{depth in m}

  \item{light}{light level}

  \item{temperature}{temperature in C}

  \item{velocity}{velocity in m/s}
}


\details{The data is a subset of an entire TDR record, so it is not
  meant to make any inferences from this particular
  individual/deployment.}


\seealso{\code{\link{readTDR}}}


\source{Sebastian P. Luque, Christophe Guinet, John P.Y. Arnould}


\author{Sebastian P. Luque <\email{spluque@gmail.com}>}


\keyword{datasets}
