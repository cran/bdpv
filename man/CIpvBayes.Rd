\name{CIpvBayes}
\alias{CIpvBI}
\alias{CIpvBII}

\title{
Confidence intervals for negative and positive predictive values in a case-control setting by simulation from the posterior distribution.
}
\description{
Computes confidence intervals for negative and positive predictive values by simulation from the posterior beta-distribution (Stamey and Holt, 2010),
 assuming a case-control design to estimate sensitivity and specificity, while prevalence estimates of an external study and/or prior knowledge concerning
 prevalence may be introduced additionally.
}
\usage{
CIpvBI(x1, x0, pr, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"),
 B=5000, shapes1=c(1,1), shapes0=c(1,1), ...)

CIpvBII(x1, x0, xpr, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"),
 B=5000, shapes1=c(1,1), shapes0=c(1,1), shapespr=c(1,1), ...)
}

\arguments{

  \item{x1}{
 A vector of two (integer) values, specifying the observed number of positive (\code{x1[1]}) and negative (\code{x1[2]}) test results in the group of true positives.
}
  \item{x0}{
 A vector of two (integer) values, specifying the observed number of positive (\code{x0[1]}) and negative (\code{x0[2]}) test results in the group of true negatives.
}
  \item{pr}{
A single numeric value between 0 and 1, defining an assumed fixed (known) prevalence (for \code{CIpvBI}), where prevalence is the proportion of positives in the population.
}
  \item{xpr}{
 An optional vector of two (integer) values, specifying the observed number of positive (\code{xpr[1]}) and negative (\code{xpr[2]}) outcomes from an external study that allows to estimate the prevalence of positives in the population of interest.
}
  \item{conf.level}{
The confidence level, a single numeric value between 0 and 1, defaults to 0.95
}
  \item{alternative}{
A character string specifying whether two-sided (\code{"two.sided"}), only lower bounds (\code{"greater"}) or only upper bounds (\code{"less"}) shall be calculated.
}
  \item{B}{
A single integer, the number of samples from the posterior to be drawn.
}
  \item{shapes1}{
Two positive numbers, the shape parameters (a,b) of the beta prior for the sensitivity, by default a flat beta prior (a=1, b=1) is used.
}
  \item{shapes0}{
Two positive numbers, the shape parameters (a,b) of the beta prior for (1-specificity), by default a flat beta prior (a=1, b=1) is used. Note, that this definition differs from that in Stamey and Holt(2010), where the prior is defined for the specificity directly.
}
  \item{shapespr}{
Two positive numbers, the shape parameters (a,b) of the beta prior for the prevalence, by default a flat beta prior (a=1, b=1) is used. For \code{CIpvBII} only.
}

  \item{...}{Arguments to be passed to \code{quantile(), other arguments are ignored without warning. }.
}


}
\details{
\code{CIpvBI} implements the method refered to as Bayes I in Stamey and Holt (2010), \code{CIpvBI} implements the method refered to as Bayes II in Stamey and Holt (2010), Equation (2) and following description (p. 103-104).

}
\value{
A list with elements
\item{conf.int }{the confidence bounds}
\item{estimate }{the point estimate}
\item{tab }{a 2x2 matrix showing how the input data in terms of true positives and true negatives}
}
\references{
\emph{Stamey JD and Holt MM (2010).} Bayesian interval estimation for predictive values for case-control studies. Communications in Statistics - Simulation and Computation. 39:1, 101-110.
}
\author{
Frank Schaarschmidt
}

\examples{

# example data: Stamey and Holt, Table 8 (page 108)
#	Diseased
# Test	D=1	D=0
# T=1	240	87
# T=0	178	288
#n1,n0:	418	375


# reproduce the results for the Bayes I method
# in Stamey and Holt (2010), Table 9, page 108

# assuming known prevalence 0.03 
# ppv 0.0591, 0.0860
# npv 0.9810, 0.9850
CIpvBI( x1=c(240,178), x0=c(87,288), pr=0.03)

# assuming known prevalence 0.04 
# ppv 0.0779, 0.1111
# npv 0.9745, 0.9800
CIpvBI( x1=c(240,178), x0=c(87,288), pr=0.04)

# compare with standard logit intervals
tab <- cbind( x1=c(240,178), x0=c(87,288))
tab
BDtest(tab, pr=0.03)
BDtest(tab, pr=0.04)


# reproduce the results for the Bayes II method 
# in Stamey and Holt (2010), Table 9, page 108

CIpvBII( x1=c(240,178), x0=c(87,288),  shapespr=c(16,486))

CIpvBII( x1=c(240,178), x0=c(87,288), shapespr=c(21,481))

}

\keyword{htest}
