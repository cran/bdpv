\name{CInpvppv}
\alias{CInpvppv}
\alias{CIlnpv}
\alias{CIlppv}
\alias{CIlnpvak}
\alias{CIlppvak}
\alias{CombCInpv}
\alias{CombCIppv}

\title{
Asymptotic confidence intervals for negative and positive predictive values.
}
\description{
Computes asymptotic confidence intervals for negative and positive predictive values under the assumption of binomial sampling and known prevalence, according to Mercaldo et al. (2007). The standard logit intervals and and adjusted version are available, where the standard logit intervals are recommended. 
}
\usage{
CIlnpv(x0, x1, p, conf.level = 0.95, 
alternative = c("two.sided", "less", "greater"))

CIlppv(x0, x1, p, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))

CIlnpvak(x0, x1, p, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))

CIlppvak(x0, x1, p, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))

CombCInpv(x0, x1, p, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))

CombCIppv(x0, x1, p, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))
}

\arguments{
  \item{x0}{
 A vector of two (integer) values, specifying the observed number of positive (\code{x0[1]}) and negative (\code{x0[2]}) outcomes in the group of true negatives.
}
  \item{x1}{
 A vector of two (integer) values, specifying the observed number of positive (\code{x1[1]}) and negative (\code{x1[2]}) outcomes in the group of true positives.
}
  \item{p}{
The assumed prevalence, a single numeric value between 0 and 1.
}
  \item{conf.level}{
The confidence level, a single numeric value between 0 and 1, defaults to 0.95
}
  \item{alternative}{
A character string specifying whether two-sided (\code{"two.sided"}), only lower bounds (\code{"greater"}) or only upper bounds (\code{"less"}) shall be calculated.
}
}
\details{

\code{CIlnpv} and \code{CIlppv} implement the standard logit intervals for NPV and PPV, Section 2.2, Eq.(8)-Eq.(11) in Mercaldo et al. (2007). \code{CIlnpvak} and \code{CIlppval} implement the logit intervals for NPV and PPV with adjusted estimates according to Table II in Mercaldo et al. (2007). The standard logit intervals have better properties, but are not defined in a number of extreme outcomes. The adjusted logit methods do always produce intervals, but have worse frequentist properties (Mercaldo et al. 2007). The functions \code{CombCInpv}, \code{CombCIppv} combine both methods by computing the stdnard logit method when possible and computing the adjusted methods in those cases where the standard method is not defined. These functions are meant to facilitate simulation, e.g. in \code{\link{simPV}}, \code{\link{simPVmat}}.
}
\value{
A list with elements
\item{conf.int }{the confidence bounds}
\item{estimate }{the point estimate}

}
\references{
\emph{Mercaldo ND, Lau KF, Zhou XH (2007).} Confidence intervals for predictive values with an emphasis to case-control studies. Statistics in Medicine 26: 2170-2183.
}
\author{
Frank Schaarschmidt
}
\note{
These functions are meant for internal use. There is not much checking for the validity of input.
}

\seealso{
 \code{\link{BDtest}} as a user level function
}
\examples{
CIlnpv(x0=c(87,288), x1=c(240,178), p=0.03,
 conf.level = 0.95, alternative = "two.sided")

CIlppv(x0=c(87,288), x1=c(240,178), p=0.03,
 conf.level = 0.95, alternative = "two.sided")

CIlnpvak(x0=c(87,288), x1=c(240,178), p=0.03,
 conf.level = 0.95, alternative = "two.sided")

CIlppvak(x0=c(87,288), x1=c(240,178), p=0.03,
 conf.level = 0.95, alternative = "two.sided")

}

\keyword{htest}
