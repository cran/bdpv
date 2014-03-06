\name{BDtest}
\alias{BDtest}

\title{
Computing confidence intervals for sensitivity, specificity and predictive values assuming a case-control study.
}
\description{
This function computes confidence intervals for negative and positive predictive values. Confidence intervals for sensitivity, specificity are computed for completeness.
 All methods assume that data are obtained by binomial sampling, with the number of true positives and true negatives in the study fixed by design.
 The methods to compute negative and positive predictive values (NPV, PPV) assume that prevalence is a known quantity, based on external knowledge.

}
\usage{
BDtest(xmat, pr, conf.level = 0.95)
}

\arguments{
  \item{xmat}{ A 2x2 table with 4 (integer) values, where the first column (\code{xmat[,1]}) represents the numbers of positive and negative results in the group of true positives, and the second column (\code{xmat[,2]}) contains the numbers of positive and negative results in the group of true negatives, i.e. the first row contains numbers of positive results and the second row the number of negative results.
}
  \item{pr}{
 A single numeric value between 0 and 1, specifying the assumed prevalence.
}
  \item{conf.level}{
 A single numeric value between 0 amd 1, specifying the nominal confidence level. 
}
}
\details{
The exact, conservative Clopper Pearson (1934) method is used to compute intervals for the sensitivty and specificity. The asymptotic standard logit intervals (Mercaldo et al. 2007) are used to compute intervals for the predictive values. In case that the table contains any 0, the adjusted logit intervals (Mercaldo et al. 2007) are returned instead to compute intervals for the predictive values.
}
\value{
A list containing:
\item{INDAT}{a data.frame containing the input 2x2 table}
\item{SESPDAT}{a data.frame with four columns containing estimates, lower limit and two.sided interval for the sensitivity and specificity (1. and 2. row)}
\item{PPVNPVDAT}{a data.frame with four columns containing estimates, lower limit and two.sided interval for the NPV and PPV (1. and 2. row)}
}
\references{
\emph{Mercaldo ND, Lau KF, Zhou XH (2007).} Confidence intervals for predictive values with an emphasis to case-control studies. Statistics in Medicine 26:2170-2183.
}
\author{
Frank Schaarschmidt
}
\seealso{
\code{\link{CInpvppv}} for the internally used methods to compute the intervals for predictive values, 
}
\examples{

# Reproduce the standard logit interval results in 
# Table IX, Mercaldo et al.(2007)

# 1) Example data: Mercaldo et al.(2007), Table VIII:

Tab8<-matrix(c(240, 178, 87, 288), ncol=2)
colnames(Tab8)<-c("Case","Control")
rownames(Tab8)<-c("ApoEe4plus","ApoEe4minus")
Tab8

# Assuming prevalence=0.03
BDtest(xmat=Tab8, pr=0.03, conf.level = 0.95)

# Assuming prevalence=0.5
BDtest(xmat=Tab8, pr=0.5, conf.level = 0.95)

}

\keyword{ htest }
