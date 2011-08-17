\name{pvhelper}
\alias{npv}
\alias{ppv}
\alias{logitnpv}
\alias{logitppv}
\alias{setil}
\alias{sptil}
\alias{varlnpv}
\alias{varlppv}

\title{Some utility functions for computing confidence intervals for predictive values.}
\description{
For internal use.
}
\usage{
npv(p, se, sp)
ppv(p, se, sp)
logitnpv(p, se, sp)
logitppv(p, se, sp)
setil(x1, k)
sptil(x0, k)
varlnpv(x0, x1, k)
varlppv(x0, x1, k, p)
}
\arguments{
  \item{p}{numeric, the prevalence}
  \item{se}{numeric, the sensitivity}
  \item{sp}{numeric, the specificity}
  \item{x1}{(integer) vector, results in the positive group}
  \item{x0}{(integer) vector, results in the negative group}
  \item{k}{numeric, for the continuity correction}
}
\details{For internal use.}

\keyword{ misc }