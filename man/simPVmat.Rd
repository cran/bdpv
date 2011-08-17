\name{simPVmat}
\alias{simPVmat}
\title{Simulate performance of confidence intervals for predictive values}

\description{Simulate the power (probability to exclude NPV0/PPV0), the coverage probability, and 0.1, 0.2, and 0.5-quantiles of the distribution of (lower!) asymptotic confidence bounds for predictive values. Different experimental setups may be compared. The function draws data under the binomial assumption and computes the asymptotic confidence bounds (lower bounds only!) for the positive and negative predictive values.}

\usage{
simPVmat(se, sp, pr, n1, n0, NPV0, PPV0,
 conf.level = 0.95, NSIM = 500, setnames = NULL)
}

\arguments{
  \item{se}{a (vector of) numeric value(s), specifying sensitivity}
  \item{sp}{a (vector of) numeric value(s), specifying specitivity}
  \item{pr}{a (vector of) numeric value(s), specifying prevalence}
  \item{n1}{a (vector of integer) value(s), specifying the number of truely positive compounds in the trial}
  \item{n0}{a (vector of integer) value(s), specifying the number of truely negative compounds in the trial}
  \item{NPV0}{a (vector of) numeric value(s), specifying the hypothesized negative predictive value (NPV assumed under H0)}
  \item{PPV0}{a (vector of) numeric value(s), specifying the hypothesized positive predictive value (PPV assumed under H0)}
  \item{conf.level}{a single numeric value, the confidence level}
  \item{NSIM}{a single (integer) value, the number of simulations to be run}
  \item{setnames}{optional character vector to the parameter sets in the output}
}

\details{
The vector or single values in se, sp, pr, n1, n0, NPV0, PPV0 are put together (shorter vectors recycled to the length of longest vectors). Then each of the resulting parameter settings is simulated as described in \code{\link{simPV}}
}

\value{
A list with elements
\item{INDAT}{a dataframe with rows showing the sets of parameters build from the input values and columns: se, sp, pr, NPV0, PPV0, n1, n0, n (total sample size)}
 \item{NPV}{a matrix with simulation results for the negative predictive value}
 \item{PPV}{a matrix with simulation results for the positive predictive value}
 \item{NSIM}{number of suimulations}
 \item{conf.level}{nominal confidence level}
}

\author{Frank Schaarschmidt}

\seealso{This function is meantb to check small sample results obtained by the asymptotci formulas for experimental design from \code{\link{nPV}}, \code{\link{nNPV}}, \code{\link{nPPV}}}

\examples{

simPVmat(se=0.8, sp=0.95, pr=1/16,
 n1=c(177, 181), n0=c(554, 87), NPV0=0.98, PPV0=c(0.4, 0.25))


simPVmat(se=0.8, sp=0.95, pr=c(0.05,0.0625, 0.075, 0.1),
 n1=177, n0=554, NPV0=0.98, PPV0=0.4)



}

\keyword{ htest }
\concept{ power }
