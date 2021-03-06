#' @title Computes run expectancy.
#' 
#' @description Find the Run Expectancy for a given state.
#' 
#' @details Given a data set, returns a function giving the run expectancy for any base/out state
#' 
#' @param data an MLBAM dataset
#' @param drop.incomplete a LOGICAL indicating whether incomplete innings (e.g. walk-off innings)
#' should be excluded from the run expectancy model.  Default is TRUE.  
#' @param ... currently ignored
#' 
#' @return A function that takes two arguments: baseCode and outs, and returns a run value 
#' 
#' @note baseCode takes on integer values between 0 and 7 with each value correpsonding to a unique 
#' base state (e.g. runner on second, runners on first and third, bases loaded).  The baseCode is the binary representation of the
#'  base state with first, second, and third base represented by 1, 2, and 4, respectively. 
#'   For example, runners on second and third corresponds to a baseCode of 6 (i.e. 2 + 4 = 6).  
#'
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
        ds = dplyr::filter_(data, ~outsInInning == 3)
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

