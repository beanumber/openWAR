\name{getCon}
\alias{getCon}
\title{Get a RMySQL connection to the database}
\usage{
  getCon()
}
\description{
  Get a RMySQL connection to the central database
}
\details{
  This function creates a connection to the database that
  houses WAR data.
}
\examples{
con = getCon()
getWAR(con)
}

