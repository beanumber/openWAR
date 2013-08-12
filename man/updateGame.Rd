\name{updateGame}
\alias{updateGame}
\title{updateGame}
\usage{
  updateGame(gameId.vec, data, ...)
}
\arguments{
  \item{gameId}{A valid MLBAM gameId}

  \item{data}{a data.frame returned by getData()}
}
\value{
  a data.frame
}
\description{
  Replaces data from a single game
}
\details{
  Deletes, and then rbinds fresh information from a
  particular game.
}
\examples{
getData(start = "2013-03-31", end = "2013-07-14")
ds = updateGame("gid_2013_04_16_nynmlb_colmlb_2")
}

