\name{plotnPV}
\alias{plotnPV}

\title{
Plot experimental design for different setting in a single figure.
}
\description{
The function creates a plot from the results of the function \code{nPV}.
}
\usage{
plotnPV(x, NPVpar = NULL, PPVpar = NULL, legpar = NULL, ...)
}

\arguments{
  \item{x}{ an object of class \code{"nPV"} as can be obtained by calling function \code{\link{nPV}}}
  \item{NPVpar}{ a named list which specifies plot parameters for the negative predictive values, possible are \code{lty}, \code{lwd}, \code{col}, \code{pch} }
  \item{PPVpar}{ a named list which specifies plot parameters for the positive predictive values, possible are \code{lty}, \code{lwd}, \code{col}, \code{pch} }
  \item{legpar}{ a named list to pass arguments to the \code{legend}. See \code{?legend} for the possible arguments.}
  \item{\dots}{ further arguments to be passed to \code{plot}}
}
\details{
Required sample sizes for different experimental settings and prevalences, needed to achieve a prespecified power can be calculated in dependence of the proportion of true negative and true positive compounds in the validation set, using function \code{\link{nPV}}. This function draws a plot with the proportion of positive on x and the total sample size on y, combining all parameter settings in one plot. Parameter settings my be distinguished by\code{lty}, \code{lwd}, \code{col}, \code{pch} in \code{NPVpar} and \code{PPVpar}. By default a legend is drawn which can be further modified in \code{legpar}.
}
\value{A plot.}

\references{
\emph{Steinberg DM, Fine J, Chappell R (2009).} Sample size for positive and negaitive predictiove value in diagnostic research using case-control designs. Biostatoistics 10, 1, 94-105.
}
\author{
Frank Schaarschmidt
}

\seealso{
\code{\link{plotnPV2}} for a plot with separate subplots for each parameter setting
}
\examples{


TEST<-nPV(se=c(0.9, 0.92, 0.94, 0.96, 0.98), sp=c(0.98, 0.96, 0.94, 0.92, 0.90),
 pr=0.12, NPV0=0.98, PPV0=0.4, NPVpower = 0.8, PPVpower = 0.8,
 rangeP = c(0.05, 0.95), nsteps = 100, alpha = 0.05)

plotnPV(TEST)

# plot parameters maybe introduced via ...
# the legend maybe modified via legpar:

plotnPV(TEST, log="y", legpar=list(x=0.6))

# own colour definitions
plotnPV(TEST, NPVpar=list(col=1:6, lwd=2, lty=1),
 PPVpar=list(col=1:6, lwd=2, lty=3))


}

\keyword{hplot}
