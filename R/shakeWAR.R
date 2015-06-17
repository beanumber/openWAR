#' @title shakeWAR
#' 
#' @description resample a data.frame to obtain variance estimates for WAR
#' 
#' @details Resamples the rows of an MLBAM data set
#' 
#' @param data An MLBAM data.frame 
#' @param N the number of resamples (default 10)
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
#' res = shakeWAR(May, resample='plays', N=10)
#' summary(res)
#' }
#' 

shakeWAR <- function(data, N = 10, ...) UseMethod("shakeWAR")

#' @rdname shakeWAR
#' @export shakeWAR.list
#' 
#' @examples 
#' owar <- shakeWAR(MayProcessed, N = 5)
#' summary(owar)

shakeWAR.list <- function(data, N = 10, ...) {
  if (!"openWAR" %in% names(data)) {
    stop("the 'data' list does not contain processed openWAR!")
  }
  return(shakeWAR.openWARPlays(data$openWAR, N, ...))
}

#' @rdname shakeWAR
#' @export shakeWAR.openWARPlays
#' 
#' @examples 
#' madeWAR <- makeWAR(May)
#' owar <- shakeWAR(madeWAR$openWAR, N = 5)
#' summary(owar)

shakeWAR.openWARPlays <- function(data, N = 10, ...) {
  # assume the models are fixed, and resample the RAA values this captures the sampling error
  bstrap = mosaic.do(N) * getWAR(mosaic::resample(data), verbose = FALSE)
  # bstrap should be a data.frame of class 'do.openWARPlayers'
  class(bstrap) <- c("do.openWARPlayers", class(bstrap))
  # with roughly N * M rows, where M is the numbers of players
  return(bstrap)
}

#' @param resample An element of \code{c('plays', 'models', 'both')}. Currently only \code{plays}
#' and \code{both} are implemented. 
#' 
#' @rdname shakeWAR
#' @export shakeWAR.GameDayPlays
#' 
#' @examples 
#' owar <- shakeWAR(May, N = 7)
#' summary(owar)
#' \dontrun{
#' owar <- shakeWAR(May, N = 2, resample = "both")
#' summary(owar)
#' }


shakeWAR.GameDayPlays <- function(data, N = 10, ...) {
  dots <- list(...)
  if (!"resample" %in% dots) {
    resample = "plays"
  } 
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
    return(shakeWAR.openWARPlays(ext$openWAR, N, ...))
  }
} 
