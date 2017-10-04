#' Estimates the variance of openWAR
#' 
#' @description Resample to obtain variance estimates for WAR.
#' 
#' @details The general idea is to use resampling techniques to compute
#' variance estimates for openWAR. The \code{resample} argument controls 
#' how that resampling is done. 
#' 
#' For \code{resample = 'plays'}, \code{\link{makeWAR}} is run on the actual
#' plays passed in the \code{data} argument, if it hasn't been computed already.
#' The resulting RAA values (for each play) are an estimate of what actually happened. 
#' These RAA values are resampled \code{N} times. 
#' 
#' \code{resample = 'models'} is not currently implemented, but in future versions,
#' this option would allow you to resample the rows of the original data, run 
#' \code{\link{makeWAR}} on each resampled data set, collect all of the models
#' generated during that process, and then evaluate the original data using each
#' of the resampled models. This would allow you isolate the modeling error. 
#' 
#' For \code{resample = 'both'}, the plays are resampled, and \code{\link{makeWAR}}
#' is computed on each resampled data set. Note that this is both computationally
#' and memory intensive. 
#' 
#' @param data An object that contains \code{\link{GameDayPlays}} data or data that has already been processed by \code{\link{makeWAR}}.  
#' @param N the number of resamples (default 10)
#' @param resample An element of \code{c('plays', 'models', 'both')}. Currently only \code{plays}
#' and \code{both} are implemented. Only works with \code{\link{shakeWAR.GameDayPlays}} method.
#' @param ... additional arguments passed to \code{\link{shakeWAR}} methods
#' 
#' @return a \code{do.\link{openWARPlayers}} data frame with RAA values 
#' @export 
#' @importFrom mosaic resample
#' @importFrom dplyr mutate
#' @examples
#' 
#' \dontrun{
#' res <- shakeWAR(May, resample='plays', N=5)
#' summary(res)
#' }
#' 

shakeWAR <- function(data, N = 10, resample = "plays", ...) UseMethod("shakeWAR")

#' @rdname shakeWAR
#' @export
#' @method shakeWAR list
#' 
#' @examples 
#' \dontrun{
#' owar <- shakeWAR(MayProcessed, N = 5)
#' summary(owar)
#' }

shakeWAR.list <- function(data, N = 10, resample = "plays", ...) {
  if (!"openWARPlays" %in% names(data)) {
    stop("the 'data' list does not contain processed openWAR!")
  }
  return(shakeWAR(data$openWAR, N, resample, ...))
}

#' @rdname shakeWAR
#' @export 
#' @method shakeWAR openWARPlays
#' 
#' @examples 
#' \dontrun{
#' owar <- shakeWAR(MayProcessed$openWAR, N = 5)
#' summary(owar)
#' }

shakeWAR.openWARPlays <- function(data, N = 10, resample = "plays", ...) {
  # assume the models are fixed, and resample the RAA values this captures the sampling error
  bstrap <- mosaic.do(N) * getWAR(mosaic::resample(data), verbose = TRUE) %>% 
      mutate_(playerId = ~as.numeric(playerId))
  # bstrap should be a data.frame of class 'do.openWARPlayers'
  class(bstrap) <- c("do.openWARPlayers", class(bstrap))
  # with roughly N * M rows, where M is the numbers of players
  return(bstrap)
}

#' @rdname shakeWAR
#' @export
#' @method shakeWAR GameDayPlays
#' 
#' @examples 
#' \dontrun{
#' owar <- shakeWAR(May, N = 7)
#' summary(owar)
#' owar <- shakeWAR(May, N = 2, resample = "both")
#' summary(owar)
#' }


shakeWAR.GameDayPlays <- function(data, N = 10, resample = "plays", ...) {
  if (resample == "both") {
    # resample the actual plays AND rebuild the models each time this captures both measurement error and sampling error
    bstrap <- mosaic.do(N) * getWAR(makeWAR(mosaic::resample(data), low.memory = TRUE)$openWAR)
    #madeWar <- makeWAR(data)
    #bstrap <- mosaic::do(N) * getWAR(mosaic::resample(madeWar), verbose = FALSE)
    # bstrap should be a data.frame of class 'do.openWARPlayers'
    class(bstrap) <- c("do.openWARPlayers", class(bstrap))
    # with roughly N * M rows, where M is the numbers of players
    return(bstrap)
  } else {
    # resample = 'plays'
    ext = makeWAR(data, verbose = FALSE, low.memory = TRUE)
    return(shakeWAR(ext$openWAR, N, resample = "plays", ...))
  }
} 
