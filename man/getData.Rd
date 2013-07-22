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

  \item{drop.suspended}{a LOGICAL indicating whether games
  with fewer than 5 innings should be excluded}
}
\value{
  a data.frame consisting of play-by-play data
}
\description{
  Retrieves MLBAM GameDay files for a time interval
}
\details{
  Given a beginning and end date, this function will
  retrieve data from the GameDay server, and process them
  into a single data.frame
}
\examples{
ds = getData()
getData(start = "2013-05-21", end = Sys.Date())
}

