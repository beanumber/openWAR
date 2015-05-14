#' @title getRunEx
#' 
#' @description Find the Run Expectancy for a given state
#' 
#' @details Given a data set, return a function giving the run expectancy for any base/out state
#' 
#' @param data an MLBAM dataset
#' @param drop.incomplete a LOGICAL indicating whether incomplete innings (e.g. walk-off innings)
#' should be excluded from the run expectancy model
#' 
#' @return A function that takes two arguments: baseCode and outs, and return a run value 
#' 
#' @importFrom mosaic makeFun
#' 
#' @export
#' @examples
#' 
#' ds = data(MLBAM2013)
#' fit.rem = getRunEx(ds)
#' 
#' fit.rem(0,0)
#' fit.rem(4,1)
#' 

getRunEx = function (data, drop.incomplete = TRUE, ...) {
  require(mosaic)
  if (drop.incomplete) {
    ds = subset(data, outsInInning == 3)
  } else {
    ds = data
  }
  mod = lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data=ds)
  summary(mod)
  rem = makeFun(mod)
  fit.rem = function (baseCode, outs) {
    good = baseCode %in% 0:7 & outs %in% 0:2
    out = NULL
    out[good] = rem(baseCode[good], outs[good])
    out[!good] = 0
    return(out)
  }
  return(fit.rem)
}
