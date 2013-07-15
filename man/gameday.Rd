\name{gameday}
\alias{gameday}
\alias{gameday.default}
\title{gameday}
\usage{
  gameday(gameId = "gid_2012_08_12_atlmlb_nynmlb_1", ...)
}
\arguments{
  \item{gameId}{A valid MLBAM gameId}
}
\value{
  a GameDay object, which consists of a list containing
  \item{gameId}{The ID of the game} \item{base}{the base
  URL for the game} \item{xml}{a list of XML files
  downloaded from the GameDay server} \item{ds}{the
  processed data set for use with openWAR}
}
\description{
  Convenience class for dealing with MLBAM GameDay files
}
\details{
  Each gameId corresponds to one GameDay object
}
\examples{
ds = getData()
getData(start = "2013-05-21", end = Sys.Date())
}

