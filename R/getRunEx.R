#' @title getRunEx
#' 
#' @description Find the Run Expectancy for a given state
#' 
#' @details Given a data set, returns a function giving the run expectancy for any base/out state
#' 
#' @param data an MLBAM dataset
#' @param drop.incomplete a LOGICAL indicating whether incomplete innings (e.g. walk-off innings)
#' should be excluded from the run expectancy model.  Default is TRUE.  
#' 
#' @return A function that takes two arguments: baseCode and outs, and returns a run value 
#' 
#' 
#' @export
#' @import dplyr
#' @examples
#' 
#' #Data from May 2013
#' data(May)
#' fit.rem = getRunEx(May)
#' 
#' fit.rem(0,0)
#' fit.rem(4,1)
#' 

getRunEx = function(data, drop.incomplete = TRUE, ...) {
    if (drop.incomplete) {
        ds = filter(data, outsInInning == 3)
    } else {
        ds = data
    }
    mod = lm(runsFuture ~ as.factor(startCode) * as.factor(startOuts), data = ds)
    fit.rem = function(baseCode, outs) {
      good.idx <- baseCode %in% 0:7 & outs %in% 0:2
      out <- NULL
      out[good.idx] <- predict(mod, newdata = data.frame(startCode = baseCode, startOuts = outs))
      out[!good.idx] <- 0
      return(out)
    }
    return(fit.rem)
} 

