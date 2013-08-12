#' @title exWPct
#' 
#' @description Model for a team's winning percentage
#' 
#' @details An implementation of Bill James's Pythagorean formula for a team's
#' expected winning percentage as a function of their runs scored and runs
#' allowed.  
#' 
#' @param rs A numeric representing Runs Scored
#' @param ra A numeric representing Runs Allowed
#' @param exponent A numeric representing the desired exponent
#'
#' @return A value of expected winning percentage
#' 
#' @export
#' @examples
#' # For a .500 team
#' exWPct(750, 750)
#' # Find the partial derivatives
#' require(mosaic)
#' slope.rs = D(exWPct(rs, ra, exponent = 1.83) ~ rs)
#' slope.ra = D(exWPct(rs, ra, exponent = 1.83) ~ ra)
#' # How many wins is one run in a 700 run environment?
#' runsPerWin = makeFun(1 / (162 * slope.rs(r, r)) ~ r)
#' runsPerWin(700)
#' # In what run environment is 10 runs equal to 1 win?
#' runsPerWin(742)
#' # Plot Runs per Win
#' plotFun(runsPerWin(r) ~ r, r.lim = c(300, 1200)
#' , xlab = "Runs Environment per 162 games"
#' , ylab = "Runs per Win")
#' 
exWPct <- makeFun(1 / (1 + (ra/rs)^exponent) ~ rs & ra & exponent, exponent = 2)

runsPerWin <- makeFun(1 / (162 * D(exWPct(rs, ra, exponent=1.83) ~ rs)(r, r)) ~ r & exponent)

