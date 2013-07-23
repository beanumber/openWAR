\name{getFielderRAA}
\alias{getFielderRAA}
\title{getFielderRAA}
\usage{
  getFielderRAA(data)
}
\arguments{
  \item{data}{An MLBAM data.frame of BIPs}
}
\value{
  a matrix of Runs Above Average (RAA)
}
\description{
  Determine the RAA of the fielders
}
\details{
  RAA is the residuals from a simple fielding model
}
\examples{
ds = getData()
ds$resp.field = getFieldResp(ds)
}

