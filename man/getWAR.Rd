\name{getWAR}
\alias{getWAR}
\title{getWAR}
\usage{
  getWAR(data, nteams = 30, verbose = TRUE, ...)
}
\arguments{
  \item{data}{An object of class \code{"openWARPlays"}}
}
\description{
  Tabulates WAR
}
\details{
  Compute each player's WAR, given their RAA values
}
\examples{
ds = getData()
out = makeWAR(ds)
raa = getRAA(out$openWAR)
war = getWAR(raa)
}

