\name{getData.gameday}
\alias{fetchData}
\alias{getData.gameday}
\title{getData}
\usage{
  getData.gameday(gd, use.cache = TRUE)
}
\arguments{
  \item{gameId}{A valid MLBAM gameId}

  \item{use.cache}{logical indicating whether GameDay
  should be localled cached (TRUE by default)}
}
\value{
  a data.frame consisting of play-by-play data for that
  game
}
\description{
  Retrieves MLBAM GameDay files from the Internet
}
\details{
  Given a valid gameId, this function will retrieve XML
  file from the GameDay server, and process them into a
  single data.frame
}
\examples{
gd = gameday()
ds = getData(gd)
}

