\name{tabulate}
\alias{tabulate}
\title{tabulate.MLBAMData}
\usage{
  tabulate(data)
}
\arguments{
  \item{data}{An MLBAMData set}
}
\value{
  A data.frame of seasonal totals for each team
}
\description{
  Summarize MLBAM data
}
\details{
  Tabulates Lahman-style statistics by team for the
  contents of an MLBAMData set.
}
\examples{
ds = getData()
tabulate(ds)
}

