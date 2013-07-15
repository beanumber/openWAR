\name{warplot}
\alias{warplot}
\title{warplot}
\usage{
  warplot(batterIds, data, N = 5000, ...)
}
\arguments{
  \item{batterId}{A vector of valid MLBAM player ID present
  in the data argument}

  \item{data}{}
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

