\name{plotnPV2}
\alias{plotnPV2}
\title{Plot experimental design for different settings in a set of sub figure.}

\description{The function creates a plot from the results of the function \code{\link{nPV}}.}

\usage{
plotnPV2(x, NPVlty = 1, PPVlty = 3, ...)
}

\arguments{
  \item{x}{ an object of class \code{"nPV"} as can be obtained by calling function \code{\link{nPV}}}
  \item{NPVlty}{single integer value, the linetype for NPV sample size, see \code{par} for the options}
  \item{PPVlty}{single integer value, the linetype for PPV sample size, see \code{par} for the options}
  \item{\dots}{further arguments to be passed to \code{plot}}
}
\details{
Required sample sizes for different experimental settings and prevalences, needed to achieve a prespecified power can be calculated in dependence of the proportion of true negative and true positive compounds in the validation set, using function \code{\link{nPV}}. This function draws a plot with the proportion of true positives on x and the total sample size on y, combining all parameter settings in one plot.

Note that for huge numbers of setting this should not work.
}
\value{
A plot.
}
\references{
\emph{Steinberg DM, Fine J, Chappell R (2009).} Sample size for positive and negaitive predictiove value in diagnostic research using case-control designs. Biostatoistics 10, 1, 94-105.
}
\author{
Frank Schaarschmidt
}

\seealso{
 \code{\link{plotnPV}}, for sample sizes for several settings in one figure
}
\examples{

TEST<-nPV(se=c(0.9, 0.92, 0.94, 0.96, 0.98), sp=c(0.98, 0.96, 0.94, 0.92, 0.90),
 pr=0.12, NPV0=0.98, PPV0=0.4, NPVpower = 0.8, PPVpower = 0.8,
 rangeP = c(0.05, 0.95), nsteps = 20, alpha = 0.05)

plotnPV2(TEST, log="x")
}

\keyword{hplot}

