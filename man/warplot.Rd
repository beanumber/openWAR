\name{warplot}
\alias{warplot}
\title{warplot}
\usage{
  warplot(playerIds, data, N = 5000, ...)
}
\arguments{
  \item{playerIds}{A vector of valid MLBAM player IDs
  present in the data argument}

  \item{data}{A dataset}

  \item{N}{the number of bootstrapped estimates}
}
\value{
  an xyplot()
}
\description{
  Visualize WAR
}
\details{
  Density Plot for WAR estimates
}
\examples{
ds = getData()
warplot(ds)
}

