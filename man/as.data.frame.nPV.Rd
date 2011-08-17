\name{as.data.frame.nPV}
\alias{as.data.frame.nPV}
\title{
Coerce results of "nPV" to a data.frame.
}
\description{
Coerce the possibly long sample size tables resulting from calling "nPV" to a data.frame.
}
\usage{
\method{as.data.frame}{nPV}(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{"nPV"}}
  \item{\dots}{further arguments to be passed to \code{as.data.frame}}
}
\details{
The lengthy lists in the output \code{\link{nPV}}, item \code{nlist} are coerced to a data.frame with columns propP, and the different NPV/PPV sample sizes for each of the parameters settings following.
}


\keyword{ misc }
