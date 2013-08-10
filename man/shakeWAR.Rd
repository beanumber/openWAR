\name{shakeWAR}
\alias{shakeWAR}
\title{shakeWAR}
\usage{
  shakeWAR(data, N = 10, ...)
}
\arguments{
  \item{data}{An MLBAM data.frame}

  \item{N}{the number of resamples (default 5000)}
}
\value{
  a data.frame with RAA values
}
\description{
  resample a data.frame to obtain variance estimate for WAR
}
\details{
  Resamples the rows of an MLBAM data set
}
\examples{
ds = getData()
ds = makeWAR()
res = shakeWAR(ds)
}

