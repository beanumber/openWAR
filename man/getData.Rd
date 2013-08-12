\name{getData}
\alias{getData}
\title{getData}
\usage{
  getData(start = Sys.Date() - 1, end = NULL,
    gameIds = NULL, drop.suspended = TRUE)
}
\arguments{
  \item{start}{A valid date in yyyy-mm-dd format (default
  today)}

  \item{end}{A valid date in yyyy-mm-dd format (default
  start)}

  \item{drop.suspended}{Logical indicating whether games
  with fewer than 5 innings should be excluded}
}
\value{
  a data.frame consisting of play-by-play data
}
\description{
  Retrieves MLBAM GameDay files for a specified time
  interval
}
\details{
  Given a beginning and end date, this function will
  retrieve data from the MLABM GameDay server, and process
  them into a single data.frame.
}
\examples{
# Get data from yesterday
ds = getData()
# Get data from the beginning of the year
ds = getData(start = "2013-03-31", end = Sys.Date())
# Get data over an arbitrary time interval
ds = getData(start = "2013-05-21", end = "2013-05-31")
}

