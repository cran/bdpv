\name{nNPVPPV}
\alias{nNPVPPV}
\alias{nNPV}
\alias{nPPV}
\alias{AOppvnpv}
\title{Asymptotic experimental design for inference on negative and positive predictive values.}

\description{For internal use. Functions to compute sample size (to reach a pre-specified power) and optimal allocation of true positives and true negatives in case-control designs for binary diagnostic tests (Mercaldo et al. 2007).}

\usage{
nNPV(propP, se, sp, prev, NPV0,
 conf.level = 0.95, power = 0.8)

nPPV(propP, se, sp, prev, PPV0,
 conf.level = 0.95, power = 0.8)

AOppvnpv(se, sp)
}

\arguments{
  \item{se}{a numeric value, specifying the expected sensitivity}
  \item{sp}{a numeric value, specifying the expected specificity}
  \item{propP}{a vector of numeric values of proportions of truely positives in the trial (n1/(n1+n0))}
  \item{prev}{a numeric value, the prevalence}
  \item{NPV0}{a numeric value, the negative predictive value to be rejected under H0: NPV>=NPV0}
  \item{PPV0}{a numeric value, the positive predictive value to be rejected under H0: PPV>=PPV0}
  \item{conf.level}{a single numeric values, the nominal confidence level (1-alpha)}
  \item{power}{a single numeric value, the power that is to be obtained}
}

\details{
The functions implement the methods described in section 3.2 of Steinberg et al.(2009), nPPV gives the solution to Eq.(3.6) and NA if necesarry conditions mentioned before are not fulfilled, 
 nNPV gives the solution to Eq.(3.8) and NA if necesarry conditions mentioned before are not fulfilled, AOppvnpv gives the optimal proportion of true poistives as are solutions to Eq.(3.4) and Eq. (3.6) for PPV and NPV, respectively.
}

\value{
For nNPV and nPPV: a list with first element
\item{n}{the (vector of) sample size (s), or NA if necessary conditions are not met}
and further elements giving the input arguments
}

\references{
\emph{Steinberg DM, Fine J, Chappell R (2009).} Sample size for positive and negative predictive value in diagnostic research using case-control designs. Biostatistics 10,1, 94-105.
}

\author{Frank Schaarschmidt}

\seealso{
For a combination of PPV and NPV experimental design see \code{\link{nPV}} and \code{\link{plotnPV}}; to validate small sample results of these asymptotic formulas, see \code{\link{simPVmat}} }
\examples{

 nPPV(propP=c(0.2,0.4,0.6,0.8), se=0.9, sp=0.9,
 prev=0.1, PPV0=0.4, conf.level=0.95, power=0.8)

 nNPV(propP=c(0.2,0.4,0.6,0.8), se=0.9, sp=0.9,
 prev=0.1, NPV0=0.95, conf.level=0.95, power=0.8)

 AOppvnpv(se=0.9, sp=0.9)
}

\keyword{ htest }
\concept{ power }
\concept{ sample size }
