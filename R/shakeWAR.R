#' @title shakeWAR
#' 
#' @description resample a data.frame to obtain variance estimates for WAR
#' 
#' @details Resamples the rows of an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' @param N the number of resamples (default 10)
#' @param resample An element of \code{c('plays', 'models', 'both')}. Currently only \code{plays}
#' and \code{both} are implemented. Only works with \code{shakeWAR.GameDayPlays} method.
#' @param ... additional arguments passed to \code{shakeWAR} methods
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
#' res = shakeWAR(May, resample='plays', N=5)
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
  if (!"openWAR" %in% names(data)) {
    stop("the 'data' list does not contain processed openWAR!")
  }
  return(shakeWAR.openWARPlays(data$openWAR, N, resample, ...))
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
  bstrap = mosaic.do(N) * getWAR(mosaic::resample(data), verbose = FALSE)
  # bstrap should be a data.frame of class 'do.openWARPlayers'
  class(bstrap) <- c("do.openWARPlayers", class(bstrap))
  # with roughly N * M rows, where M is the numbers of players
  return(bstrap)
}

#' 
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
    bstrap = mosaic.do(N) * getWAR(makeWAR(mosaic::resample(data), low.memory = TRUE)$openWAR)
    # bstrap should be a data.frame of class 'do.openWARPlayers'
    class(bstrap) <- c("do.openWARPlayers", class(bstrap))
    # with roughly N * M rows, where M is the numbers of players
    return(bstrap)
  } else {
    # resample = 'plays'
    ext = makeWAR(data, verbose = FALSE, low.memory = TRUE)
    return(shakeWAR.openWARPlays(ext$openWAR, N, resample = "plays", ...))
  }
} 
