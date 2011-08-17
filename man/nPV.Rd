\name{nPV}
\alias{nPV}

\title{
Asymptotic sample size for inference on negative and positive predictive values.
}
\description{
Functions to compute sample size (to reach a pre-specified power) and optimal allocation of true positives and true negatives in case-control designs for binary diagnostic tests (Mercaldo et al. 2007).
}

\usage{
nPV(se, sp, prev, NPV0, PPV0,
 NPVpower = 0.8, PPVpower = 0.8,
 rangeP = c(0.05, 0.95), nsteps = 20,
 alpha = 0.05, setnames = NULL)
}

\arguments{
  \item{se}{a (vector of) numeric value(s), specifying the expected sensitivity}
  \item{sp}{a (vector of) numeric value(s), specifying the expected specificity}
  \item{prev}{a (vector of) numeric value(s), specifying the prevalence}
  \item{NPV0}{a (vector of) numeric value(s), specifying the negative predictive value to be rejected under H0: NPV>=NPV0}
  \item{PPV0}{a (vector of) numeric value(s), specifying the positive predictive value to be rejected under H0: PPV>=PPV0}
  \item{NPVpower}{a (vector of) numeric value(s), the power that is to be obtained for the test H0: NPV>=NPV0}
  \item{PPVpower}{a (vector of) numeric value(s), the power that is to be obtained for the test H0: PPV>=PV0}
  \item{rangeP}{ a vector of two numeric values, giving the range of the proportion of truely positives to be considered in experimental design}
  \item{nsteps}{a single (integer) value, the number of steps in rangeP to be considered}
  \item{alpha}{a single numeric value, the type I error of the test (1-confidence level)}
  \item{setnames}{an optional vector of names for the parameter sets}
}
\details{
The function uses \code{\link{nNPVPPV}} and implement the methods described in section 3.2 of Steinberg et al.(2009).
The results for NPV are the smallest integers fulfilling Eq.(3.6) and NA if necesarry conditions mentioned before are not met, 
the results for PPV are the smallest integers fulfilling Eq.(3.8) and NA if necesarry conditions mentioned before are not met.

The arguments \code{se}, \code{sp}, \code{prev}, \code{NPV0}, \code{PPV0}, \code{NPVpower}, \code{PPVpower} can be given as vectors or single values, where shorter values are recycled to the length of the longest. The proportion of true positives is varied over \code{nstep} equidistant values over the range specified in argument \code{rangeP}.
 On each resulting parameter set, the asymptotic sample size formulas of Steinberg et al.(2009) are applied.

The result of those calculations may be plot using \code{\link{plotnPV}} and \code{\link{plotnPV2}}.
}

\value{
A list with elements
\item{outDAT }{a data.frame showing the parameter settings (in rows) and the input parameters se, sp, prev, NPV0, PPV0, NPVpower, PPVpower, trueNPV, truePPV}
\item{nlist}{ a list with an element for each parameter setting in OUTDAT, listing the results of \code{\link{nNPV}}, and \code{\link{nPPV}}}
\item{NSETS}{ a single (integer), the number of parameter sets}
\item{nsteps}{ a single (integer), the number of steps in the range of proportions of true positives}
\item{rangeP}{the input range of the proportion of true positives}
\item{propP}{the resulting sequence of proportions of true positives considered}

}
\references{
\emph{Steinberg DM, Fine J, Chappell R (2009).} Sample size for positive and negative predictive value in diagnostic research using case-control designs. Biostatistics 10,1, 94-105.

}
\author{
Frank Schaarschmidt
}


\seealso{
 \code{\link{plotnPV}} for showing the results in one graphic, and \code{\link{plotnPV}} for showing the results in a set of subgraphics,
}
\examples{

#Reproducing illustration from Section 3.4 and 4.2 in
#Steinberg et al. (2009)

FIG1<-nPV(se=0.8, sp=0.95, prev=1/16, NPV0=0.98, PPV0=0.4,
 NPVpower = 0.8, PPVpower = 0.8,
 rangeP = c(0.01, 0.99), nsteps = 100, alpha = 0.05)

FIG1

DFIG1<-as.data.frame(FIG1)

plot(x=DFIG1$propP, y=DFIG1[,2], ylim=c(0,2000), lty=1,  type="l",
 ylab="total sample size", xlab="proportion of true positives")
lines(x=DFIG1$propP, y=DFIG1[,3], lty=2 )

}

\keyword{ htest }
\keyword{ power }
\keyword{ sample size }
