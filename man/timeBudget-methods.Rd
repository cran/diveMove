\name{timeBudget-methods}
\docType{methods}

\alias{timeBudget}
\alias{timeBudget,TDRcalibrate,logical-method}

% -------------------------------------------------------------------------
\title{Describe the Time Budget of Major Activities from "TDRcalibrate"
  object.}


\description{
  Summarize the major activities recognized into a time budget.
}

\usage{
\S4method{timeBudget}{TDRcalibrate,logical}(obj, ignoreZ)

}

\arguments{
  \item{obj}{\code{"\linkS4class{TDRcalibrate}"} object.}

  \item{ignoreZ}{logical indicating whether to ignore trivial aquatic
    periods.}

}

\value{
  A data frame.
}

\details{
  Ignored trivial aquatic periods are collapsed into the enclosing dry period.
}

% -------------------------------------------------------------------------
% \examples{
% ## TODO
% }

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
