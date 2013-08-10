\name{summary.war}
\alias{summary.war}
\title{summary.war}
\usage{
  summary.war(data, ...)
}
\arguments{
  \item{playerIds}{A vector of valid MLBAM player IDs
  present in the data argument}

  \item{data}{A dataset of resampled WAR components,
  aggregated by player}
}
\value{
  a data.frame
}
\description{
  Summarize WAR
}
\details{
  Aggegrate the resampled WAR estimates
}
\examples{
ds = getData()
samp = shakeWAR(makeWAR(ds))
summary.war(samp)
}

