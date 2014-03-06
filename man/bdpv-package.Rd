\name{bdpv-package}
\alias{bdpv-package}
\alias{bdpv}
\docType{package}
\title{Confidence intervals and experimental design for negative and positive predictive values in binary diagnostic tests.}

\description{
Computing asymptotic confidence intervals for negative and positive predictive values of binary diagnostic test assuming a case-control design.
Experimental design based on asymptotic formulas and Monte Carlo simulation for hypothesis tests on predictive values, including some plot functions to explore various experimental designs.
}

\details{
\tabular{ll}{
Package: \tab bdpv\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2014-03-06\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

1) Computing confidence intervals: The function \code{\link{BDtest}} computes the asymptotic confidence intervals for negative and positive predictive value given in Mercaldo et al. (2007),
 assuming binomial sampling for obtaining estimates of sensitivity and specificity (leading to a 2x2 table with numbers of diseased and healthy fixed by design) and known prevalence.
 Alternatively, the functions \code{\link{CIpvBI}} and \code{\link{CIpvBII}} allow to simulate Bayesian intervals for negative and positive predictive values in case-control designs (Stamey and Holt, 2010),
 where prior knowledge concerning sensitivity, specificity may be included and external data and/or prior knowledge on prevalence may be included. By default, flat, non-informative priors are used,
 resulting in intervals with improved frequentist small sample performance (Stamey and Holt, 2010).

2) The function \code{\link{nPV}} uses the asymptotic formulas of Steinberg et al.(2009) to calculate the sample size necessary to reject tests witzh H0: PPV>=PPV0, H0NPV>=PNPV0, with a prespecified power in a case-control setting.
 Further necessary input arguments are sensitivity, specificity, prevalence, NPV0, PPV0, the range and number of steps of proportion of true positives in the trial.
 The results of this function can be plotted using \code{\link{plotnPV}}, \code{\link{plotnPV2}} and be somewhat edited by \code{\link{as.data.frame.nPV}}. 

3) Because the results of these functions may be misleading in small sample or extreme proportion situations, the simulation functions \code{\link{simPV}} and \code{\link{simPVmat}} allow to check power and coverage probability for given parameter stettings.  

The remaining functions are meant for internal use.

}
\author{
Frank Schaarschmidt, on behalf of the Institute of Biostatistics, LUH, Hannover, Germany
Maintainer: Frank Schaarschmidt <schaarschmidt@biostat.uni-hannover.de>
}
\references{
\emph{Steinberg DM, Fine J, Chappell R (2009).} Sample size for positive and negative predictive value in diagnostic research using case-control designs. Biostatistics 10,1, 94-105.

\emph{Mercaldo ND, Lau KF, Zhou XH (2007).} Confidence intervals for predictive values with an emphasis to case-control studies. Statistics in Medicine 26:2170-2183.

\emph{Stamey JD and Holt MM (2010).} Bayesian interval estimation for predictive values for case-control studies. Communictaions in Statistics - Simulation and Computation. 39:1, 101-110.
}

\keyword{ package }

\examples{

# 1) Example data: Mercaldo et al.(2007), Table VIII:

Tab8<-matrix(c(240, 178, 87, 288), ncol=2)
colnames(Tab8)<-c("Case","Control")
rownames(Tab8)<-c("ApoEe4plus","ApoEe4minus")
Tab8

# Assuming prevalence=0.03
BDtest(xmat=Tab8, pr=0.03, conf.level = 0.95)

# Assuming prevalence=0.5
BDtest(xmat=Tab8, pr=0.5, conf.level = 0.95)

# 2) Experimental design acc. to Steinberg et al.(2009)

TEST<-nPV(se=c(0.76, 0.78, 0.80, 0.82, 0.84),
 sp=c(0.93, 0.94, 0.95, 0.96, 0.97),
 pr=0.0625, NPV0=0.98, PPV0=0.25, NPVpower = 0.8, PPVpower = 0.8,
 rangeP = c(0.10, 0.9), nsteps = 20, alpha = 0.05)

TEST

plotnPV(TEST, log="y", legpar=list(x=0.6))


# 3) Simulation of power and coverage probability

simPVmat(se=0.8, sp=0.95, pr=0.0625, n1=c(177, 181), 
 n0=c(554, 87), NPV0=0.98, PPV0=c(0.4, 0.25))



}
