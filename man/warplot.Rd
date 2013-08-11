\name{warplot}
\alias{warplot}
\title{warplot}
\usage{
  warplot(playerIds, data, ...)
}
\arguments{
  \item{playerIds}{A vector of valid MLBAM player IDs
  present in the data argument}

  \item{data}{A data.frame resulting from shakeWAR()}
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
warplot(c(431151, 502517, 408234, 285078, 518774, 285079), data=openWAR.sim)
}

