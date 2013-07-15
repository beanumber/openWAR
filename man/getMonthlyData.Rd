\name{getMonthlyData}
\alias{getMonthlyData}
\alias{getWeeklyData}
\title{getMonthlyData}
\usage{
  getMonthlyData(yyyy = 2013, m = 5)
}
\arguments{
  \item{yyyy}{A year}

  \item{m}{a numeric value corresponding to a month}
}
\value{
  a data.frame consisting of play-by-play data
}
\description{
  Retrieves MLBAM GameDay files for a single month
}
\details{
  Given a year and month, this function will retrieve data
  from the GameDay server, and process them into a single
  data.frame
}
\examples{
ds = getData()
getMonthlyData(2-13, 5)
}

