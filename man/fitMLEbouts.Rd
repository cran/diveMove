% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllMethod.R
\name{fitMLEbouts,numeric-method}
\alias{fitMLEbouts,numeric-method}
\alias{fitMLEbouts}
\alias{fitMLEbouts,Bouts-method}
\title{Maximum Likelihood Model of mixtures of 2 or 3 Poisson Processes}
\usage{
\S4method{fitMLEbouts}{numeric}(obj, start, optim_opts0 = NULL, optim_opts1 = NULL)

\S4method{fitMLEbouts}{Bouts}(obj, start, optim_opts0 = NULL, optim_opts1 = NULL)
}
\arguments{
\item{obj}{Object of class \code{\link{Bouts}}.}

\item{start}{passed to \code{\link[stats4]{mle}}.  A row- and
column-named (2,N) matrix, as returned by \code{\link{boutinit}}.}

\item{optim_opts0}{named list of optional arguments passed to
\code{\link[stats4]{mle}} for fitting the first model with
transformed parameters.}

\item{optim_opts1}{named list of optional arguments passed to
\code{\link[stats4]{mle}} for fitting the second model with
parameters retrieved from the first model, untransformed to
original scale.}
}
\value{
An object of class \code{\link[stats4]{mle}}.
}
\description{
Functions to model a mixture of 2 random Poisson processes to identify
bouts of behaviour.  This follows Langton et al. (1995).
}
\details{
Mixtures of 2 or 3 Poisson processes are supported. Even in this
relatively simple case, it is very important to provide good starting
values for the parameters.

One useful strategy to get good starting parameter values is to proceed
in 4 steps.  First, fit a broken stick model to the log frequencies of
binned data (see \code{\link{boutinit}}), to obtain estimates of 4
parameters in a 2-process model (Sibly et al. 1990), or 6 in a
3-process model.  Second, calculate parameter(s) \var{p} from the alpha
parameters obtained from the broken stick model, to get tentative
initial values as in Langton et al. (1995). Third, obtain MLE estimates
for these parameters, but using a reparameterized version of the -log
L2 function.  Lastly, obtain the final MLE estimates for the 3
parameters by using the estimates from step 3, un-transformed back to
their original scales, maximizing the original parameterization of the
-log L2 function.

\code{\link{boutinit}} can be used to perform step 1.  Calculation of
the mixing parameters \var{p} in step 2 is trivial from these
estimates.  Function \code{\link{boutsMLEll.chooser}} defines a
reparameterized version of the -log L2 function given by Langton et
al. (1995), so can be used for step 3.  This uses a logit (see
\code{\link{logit}}) transformation of the mixing parameter \var{p},
and log transformations for both density parameters \var{lambda1} and
\var{lambda2}.  Function \code{\link{boutsMLEll.chooser}} can be used
again to define the -log L2 function corresponding to the
un-transformed model for step 4.

\code{fitMLEbouts} is the function performing the main job of
maximizing the -log L2 functions, and is essentially a wrapper around
\code{\link[stats4]{mle}}.  It only takes the -log L2 function, a list
of starting values, and the variable to be modelled, all of which are
passed to \code{\link[stats4]{mle}} for optimization.  Additionally,
any other arguments are also passed to \code{\link[stats4]{mle}}, hence
great control is provided for fitting any of the -log L2 functions.

In practice, step 3 does not pose major problems using the
reparameterized -log L2 function, but it might be useful to use method
\dQuote{L-BFGS-B} with appropriate lower and upper bounds.  Step 4 can
be a bit more problematic, because the parameters are usually on very
different scales and there can be multiple minima.  Therefore, it is
almost always the rule to use method \dQuote{L-BFGS-B}, again bounding
the parameter search, as well as passing a \code{control} list with
proper \code{parscale} for controlling the optimization.  See
\code{Note} below for useful constraints which can be tried.
}
\section{Methods (by class)}{
\itemize{
\item \code{numeric}: Fit model via MLE on numeric vector.

\item \code{Bouts}: Fit model via MLE on \code{\link{Bouts}}
object.
}}

\note{
In the case of a mixture of 2 Poisson processes, useful values for
lower bounds for the transformed negative log likelihood
reparameterization are \code{c(-2, -5, -10)}.  For the un-transformed
parameterization, useful lower bounds are \code{rep(1e-08, 3)}.  A
useful parscale argument for the latter is \code{c(1, 0.1, 0.01)}.
However, I have only tested this for cases of diving behaviour in
pinnipeds, so these suggested values may not be useful in other cases.

The lambdas can be very small for some data, particularly
\code{lambda2}, so the default \code{ndeps} in \code{\link{optim}} can
be so large as to push the search outside the bounds given.  To avoid
this problem, provide a smaller \code{ndeps} value.
}
\examples{
## Run example to retrieve random samples for two- and three-process
## Poisson mixtures with known parameters as 'Bouts' objects
## ('xbouts2', and 'xbouts3'), as well as starting values from
## broken-stick model ('startval2' and 'startval3')
utils::example("boutinit", package="diveMove", ask=FALSE)

## 2-process
opts0 <- list(method="L-BFGS-B", lower=c(-2, -5, -10))
opts1 <- list(method="L-BFGS-B", lower=c(1e-1, 1e-3, 1e-6))
bouts2.fit <- fitMLEbouts(xbouts2, start=startval2, optim_opts0=opts0,
                          optim_opts1=opts1)
plotBouts(bouts2.fit, xbouts2)

## 3-process
opts0 <- list(method="L-BFGS-B", lower=c(-5, -5, -6, -8, -12))
## We know 0 < p < 1, and can provide bounds for lambdas within an
## order of magnitude for a rough box constraint.
lo <- c(9e-2, 9e-2, 2e-3, 1e-3, 1e-5)
hi <- c(9e-1, 9.9e-1, 2e-1, 9e-2, 5e-3)
## Important to set the step size to avoid running below zero for
## the last lambda.
ndeps <- c(1e-3, 1e-3, 1e-3, 1e-3, 1e-5)
opts1 <- list(method="L-BFGS-B", lower=lo, upper=hi,
              control=list(ndeps=ndeps))
bout3.fit <- fitMLEbouts(xbouts3, start=startval3, optim_opts0=opts0,
                         optim_opts1=opts1)
bec(bout3.fit)
plotBoutsCDF(bout3.fit, xbouts3)
}
\references{
Langton, S.; Collett, D. and Sibly, R. (1995) Splitting behaviour into
bouts; a maximum likelihood approach.  Behaviour \bold{132}, 9-10.

Luque, S.P. and Guinet, C. (2007) A maximum likelihood approach for
identifying dive bouts improves accuracy, precision, and
objectivity. Behaviour, \bold{144}, 1315-1332.

Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
bouts.  Animal Behaviour \bold{39}, 63-69.
}
\author{
Sebastian P. Luque \email{spluque@gmail.com}
}
\keyword{manip}
\keyword{methods}
\keyword{models}
