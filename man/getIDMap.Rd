\name{getIDMap}
\alias{getIDMap}
\title{getIDMap}
\usage{
getIDMap()
}
\value{
A data.frame with a row for each player and a column for
each id
}
\description{
Retrieve a mapping of player Ids from different data
sources
}
\details{
Downloads the BP player list and cross-checks it with the
Lahman database
}
\examples{
ids = getIDMap()
}

