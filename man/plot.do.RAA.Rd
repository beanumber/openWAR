\name{plot.do.RAA}
\alias{plot.do.RAA}
\title{plot.do.RAA}
\usage{
  plot.do.RAA(data, playerIds = c(431151, 285079), ...)
}
\arguments{
  \item{playerIds}{A vector of valid MLBAM player IDs
  present in the data argument}

  \item{data}{A data.frame resulting from shakeWAR() of
  class \code{do.RAA}}
}
\value{
  a faceted densityplot
}
\description{
  Visualize WAR
}
\details{
  Density Plot for WAR estimates
}
\examples{
ds = getData()
# not run
openWAR = makeWAR(ds)
openWAR.sim = shakeWAR(openWAR)
plot(data=openWAR.sim, playerIds = c(431151, 502517, 408234, 285078, 518774, 285079))
}

