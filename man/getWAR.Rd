\name{getWAR}
\alias{getWAR}
\title{getWAR}
\usage{
  getWAR(data, recompute = FALSE, ...)
}
\arguments{
  \item{data}{An MLBAM data.frame}
}
\value{
  a data.frame of RAA values per player
}
\description{
  Retrieve openWAR
}
\details{
  Retrieves openWAR, aggregated by player, given an MLBAM
  data set
}
\examples{
ds = getData()
res = getWAR(ds)
}

