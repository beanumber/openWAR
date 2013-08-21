#Plot method

#' @title bbplot
#' 
#' @description Visualize Balls in Play
#' 
#' @details Plots the balls in play from GameDay data. This function will plot (x,y)-coordinates
#' with a generic baseball field plotted in the background. Other lattice options can be passed
#' to xyplot().
#' 
#' @param data An MLBAM data set with fields "our.x" and "our.y"
#' 
#' @return an xyplot() 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' bbplot(ds)

setMethod("summary", signature(object  = "WAR"),
          function(object, ...){
            dat <- object@playerWAR
            dat <- dat[order(-dat$RAA),]
            dat <- dat[1:10,]
            out <- data.frame(Name = dat$Name, RAA = dat$RAA, RAA.bat = dat$RAA.bat, RAA.pitch = dat$RAA.pitch, RAA.field = dat$RAA.field, RAA.br=dat$RAA.br)
            print(out)
                                            }
)
