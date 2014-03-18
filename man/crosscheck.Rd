\name{crosscheck}
\alias{crosscheck}
\title{crosscheck.MLBAMData}
\usage{
crosscheck(data)
}
\arguments{
  \item{data}{An MLBAM data set}
}
\value{
The ratio of the Frobenius norm of the matrix of
differences to the Frobenius norm of the matrix defined by
the Lahman database.
}
\description{
Cross-check the accuracy of the GameDay data with the
Lahman database
}
\details{
Cross-checks summary statistics with the Lahman database.
}
\examples{
ds = getData()
crosscheck(ds)
}

