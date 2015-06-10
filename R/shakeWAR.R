#' @title shakeWAR
#' 
#' @description resample a data.frame to obtain variance estimate for WAR
#' 
#' @details Resamples the rows of an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' @param resample An element of \code{c('plays', 'models', 'both')}
#' @param N the number of resamples (default 5000)
#' @param ... currently ignored
#' 
#' @return a data.frame with RAA values 
#' 
#' @export 
#' 
#' @importFrom mosaic resample
#' 
#' @examples
#' 
#' \dontrun{
#' res = shakeWAR(May, resample='plays', N=10)
#' summary(res)
#' }
#' 

shakeWAR = function(data, resample = "plays", N = 10, ...) UseMethod("shakeWAR")

#' @rdname shakeWAR
#' @export shakeWAR.GameDayPlays

shakeWAR.GameDayPlays = function(data, resample = "plays", N = 10, ...) {
  
  if (resample == "both") {
    # resample the actual plays AND rebuild the models each time this captures both measurement error and sampling error
    bstrap = mosaic.do(N) * getWAR(makeWAR(mosaic::resample(data), low.memory = TRUE)$openWAR)
  } else {
    
    ext = makeWAR(data, verbose = FALSE, low.memory = TRUE)
    # Keep track of the original data
    reality = data
    # Keep track of the models built on the original data
    reality.models = ext$models
    # Keep track of the original RAA values
    reality.raa = ext$openWAR
    
    if (resample == "plays") {
      # assume the models are fixed, and resample the RAA values this captures the sampling error
      
      # supposedly the performance of do() is really bad
      bstrap = mosaic.do(N) * getWAR(mosaic::resample(reality.raa), verbose = FALSE)
      # use replicate() instead bstrap = rdply(N, getWAR(resample(reality.raa), verbose=FALSE)) class(bstrap) =
      # c('do.openWARPlayers', class(bstrap)) } else { # to isolate the measurement error, use the models we built on the
      # resampled rows # but apply them exclusively to the real data ext.list = lapply(sims$models.used, makeWAR, data = reality,
      # verbose=FALSE) raa.list = lapply(ext.list, '[[', 'openWAR') war.list = t(lapply(raa.list, getWAR)) bstrap =
      # do.call('rbind', war.list) class(bstrap) = c('do.openWARPlayers', class(bstrap))
    }
  }
  
  # bstrap should be a data.frame of class 'do.openWARPlayers'
  class(bstrap) <- c("do.openWARPlayers", "data.frame")
  # with roughly N * M rows, where M is the numbers of players
  return(bstrap)
} 
