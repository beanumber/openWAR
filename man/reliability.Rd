\name{reliability}
\alias{reliability}
\title{reliability}
\usage{
  reliability(con, start = 2011, end = 2012)
}
\arguments{
  \item{con}{An RMySQL database connection}

  \item{start}{A numeric representing the start year of the
  interval for selecting WAR}

  \item{end}{A numeric representing the end year of the
  interval for selecting WAR}
}
\value{
  A named vector, each entry of which represents the
  correlation between WAR in that year and the previous
  year.
}
\description{
  Find the reliability (autocorrelation) of WAR in
  consecutive seasons
}
\details{
  The reliability of a statistic is defined as the
  year-to-year correlation (autocorrelation) from
  consecutive time elements. This is simply Pearson
  correlation coefficient between consecutive observations
  of the statistic for the same player. Here we find the
  correlation between a player's WAR in year $x$ with his
  WAR in year $x+1$.
}
\examples{
# Create a connection to the database
con = getCon()
# Examine the reliability of WAR
rel = reliability(con, 1980)
xyplot(rel ~ as.numeric(names(rel)), type=c("r", "l"))
}

