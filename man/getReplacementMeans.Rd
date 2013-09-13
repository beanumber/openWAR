\name{getReplacementMeans}
\alias{getReplacementMeans}
\title{getReplacementMeans}
\usage{
  getReplacementMeans(data)
}
\arguments{
  \item{data}{An openWARPlays data.frame}
}
\value{
  a vector of mean contributions per activity for
  replacement-level players
}
\description{
  Compute the mean RAA for replacement-level players
}
\details{
  Identifies replacement-level players, and then compute
  their contribution
}
\examples{
data = getData()
ds = makeWAR(data)
getReplacementMeans(ds$openWAR)
}

