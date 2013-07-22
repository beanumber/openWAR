\name{setBIPresp}
\alias{setBIPresp}
\title{setBIPresp}
\usage{
  setBIPresp(data, ...)
}
\arguments{
  \item{data}{An MLBAM data.frame}
}
\value{
  data.frame with 9 columns, each row representing a ball
  in play
}
\description{
  Find the shared responsibility for balls in play
}
\details{
  Fits 9 logistic regression models, each giving the
  probability of a fielder at one of the 9 defensive
  positions successfully converting the ball into at least
  one out.
}
\examples{
ds = getData()
setBIPresp(ds)
}

