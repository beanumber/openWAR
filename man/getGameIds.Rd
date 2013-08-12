\name{getGameIds}
\alias{getGameIds}
\title{getGameIds}
\usage{
  getGameIds(date = Sys.Date())
}
\arguments{
  \item{date}{A date in "yyyy-mm-dd" format}
}
\value{
  a vector of gameIds
}
\description{
  Retrieves MLBAM gameIds for a specified date
}
\details{
  Downloads information for a given day from the MLBAM
  website and retrieves a list of valid gameIds.
}
\examples{
getGameIds()
getGameIds("2008-05-14")
}

