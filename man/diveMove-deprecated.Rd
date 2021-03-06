% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diveMove-deprecated.R
\name{diveMove-deprecated}
\alias{diveMove-deprecated}
\alias{bouts2.nlsFUN}
\alias{bouts2.nls}
\alias{bec2}
\alias{bec3}
\alias{bouts3.nlsFUN}
\alias{bouts3.nls}
\alias{bouts2.mleFUN}
\title{Deprecated functions in diveMove}
\usage{
bouts2.nlsFUN(x, a1, lambda1, a2, lambda2)

bouts2.nls(lnfreq, start, maxiter)

bec2(fit)

bec3(fit)

bouts3.nlsFUN(x, a1, lambda1, a2, lambda2, a3, lambda3)

bouts3.nls(lnfreq, start, maxiter)

bouts2.mleFUN(x, p, lambda1, lambda2)
}
\description{
These functions are provided for compatibility with older versions of
\sQuote{diveMove} only, and will be removed (defunct) in the next
release.
}
\section{\code{bouts2.nlsFUN}}{

For \code{bouts2.nlsFUN}, use \code{\link{boutsNLSll}}.
}

\section{\code{bouts2.nls}}{

For \code{bouts2.nls}, use \code{\link{fitNLSbouts}}.
}

\section{\code{bec2}}{

For \code{bec2}, use \code{\link{bec}}.
}

\section{\code{bec3}}{

For \code{bec3}, use \code{\link{bec}}.
}

\section{\code{bouts3.nlsFUN}}{

For \code{bouts3.nlsFUN}, use \code{\link{boutsNLSll}}.
}

\section{\code{bouts3.nls}}{

For \code{bouts3.nls}, use \code{\link{fitNLSbouts}}.
}

\section{\code{bouts2.mleFUN}}{

For \code{bouts2.mleFUN}, use \code{\link{.bouts2MLEll}}.
}

\author{
Sebastian P. Luque \email{spluque@gmail.com}
}
\keyword{internal}
