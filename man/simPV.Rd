\name{simPV}
\alias{simPV}

\title{
Simulate performance of confidence intervals for predictive values in a case-control design
}
\description{
The function draws data under the binomial assumption and computes the asymptotic confidence bounds (lower bounds only!) for the positive and negative predictive values.
Output are the power (probability to exclude NPV0/PPV0), the realized coverage probability, 0.1,0.2, and 0.5-quantiles of the realized distribution of confidence bounds.
}
\usage{
simPV(se, sp, pr, n1, n0, NPV0, PPV0,
 conf.level = 0.95, NSIM = 500)
}
\arguments{
  \item{se}{a numeric value, specifying sensitivity}
  \item{sp}{a numeric value, specifying specitivity}
  \item{pr}{a numeric value, specifying prevalence}
  \item{n1}{an (integer) value, specifying the number of truely positive compounds in the trial}
  \item{n0}{an (integer) value, specifying the number of truely negative compounds in the trial}
  \item{NPV0}{a numeric value, specifying the hypothesized negative predictive value (NPV assumed under H0)}
  \item{PPV0}{a numeric value, specifying the hypothesized positive predictive value (PPV assumed under H0)}
  \item{conf.level}{a numeric value, the confidence level}
  \item{NSIM}{an (integer) value, the number of simulations to be run}
}
\details{
The function draws data under the binomial assumption in a case-control design (Mercaldo et al. 2007), where the binomial doistributions are defined by n1, n0, se, sp. Then, for each drawn data set, the asymptotic lower confidence bounds (with confidence level=1-alpha, i.e. as suitable for a one-sided test at level alpha) for the positive and negative predictive values ar computed. (Note, that the standard logit interval is replaced by the adjusted logit interval of Mercaldo et al. 2007, if the standard logit interval is not defined.)
Output are the etsimated power (observed probability that NPV0/PPV0 are excluded by the lower confidence bound), the realized coverage probability (observed probability that the true NPV/PPV are included in their interval), as well as the 0.1, 0.2, and 0.5-quantiles of the realized distribution of confidence bounds.

}
\value{
A (2x7) matrix with results for NPV and PPV in rows 1,2 respectively, and the columns giving estimates of the power to reject H0: NPV>=NPV0 / PPV>=PPV0 (pow), coverage probability (cov), the values which are excluded with 10, 20 and 50 percent probability (q10, q20, q50), as well as the true predictive values and the marginn of H0 used to calculated power.
}

\author{
Frank Schaarschmidt
}

\seealso{
\code{\link{simPVmat}} for the same function, allowing vector input for se, sp, pr, n1, n0, NPV0 and PPV0.
}
\examples{

simPV(se=0.8, sp=0.95, pr=1/16, n1=177, n0=554, NPV0=0.98, PPV0=0.4)

simPV(se=0.8, sp=0.95, pr=1/16, n1=181, n0=87, NPV0=0.98, PPV0=0.25)

}
\keyword{ htest }
\concept{ power }