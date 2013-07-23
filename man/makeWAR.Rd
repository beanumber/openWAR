\name{makeWAR}
\alias{makeWAR}
\title{makeWAR}
\usage{
  makeWAR(data, ...)
}
\arguments{
  \item{data}{An MLBAM data.frame}
}
\value{
  a data.frame
}
\description{
  Compute openWAR
}
\details{
  Computes openWAR, given an MLBAM data set
}
\examples{
ds = getData()
res = makeWAR(ds)
}

