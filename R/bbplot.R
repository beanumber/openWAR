#' @title bbplot
#' 
#' @description Visualize Balls in Play
#' 
#' @details Plot the balls in play from GameDay data
#' 
#' @param start 
#' 
#' @return an xyplot() 
#' 
#' @export
#' @examples
#' 
#' ds = getData()
#' bbplot(ds)

bbplot = function (data, ...) {
  require(mosaic)
  bgcol = "darkgray"
  ds = subset(data, !is.na(our.y) & !is.na(our.x))
  ds$event = factor(ds$event)
  plot = xyplot(our.y ~ our.x, groups=event, data=ds
         , panel = function(x,y,...) {
           panel.segments(0, 0, -400, 400, col=bgcol)   # LF line
           panel.segments(0, 0, 400, 400, col=bgcol)     # RF line
           bw = 2
           # midpoint is at (0, 127.27)
           base2.y = sqrt(90^2 + 90^2)
           panel.polygon(c(-bw, 0, bw, 0), c(base2.y, base2.y - bw, base2.y, base2.y + bw), col=bgcol)
           # back corner is 90' away on the line
           base1.x = 90 * cos(pi/4)
           base1.y = 90 * sin(pi/4)
           panel.polygon(c(base1.x, base1.x - bw, base1.x - 2*bw, base1.x - bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), col=bgcol)
           # back corner is 90' away on the line
           base3.x = 90 * cos(3*pi/4)
           panel.polygon(c(base3.x, base3.x + bw, base3.x + 2*bw, base3.x + bw), c(base1.y, base1.y - bw, base1.y, base1.y + bw), col=bgcol)
           # infield cutout is 95' from the pitcher's mound
           panel.curve(60.5 + sqrt(95^2 - x^2), from=base3.x - 26, to=base1.x + 26, col=bgcol)
           # pitching rubber
           panel.rect(-bw, 60.5 - bw/2, bw, 60.5 + bw/2, col=bgcol)
           # home plate
           panel.polygon(c(0, -8.5/12, -8.5/12, 8.5/12, 8.5/12), c(0, 8.5/12, 17/12, 17/12, 8.5/12), col=bgcol)
           # distance curves
           distances = seq(from=200, to=500, by = 100)
           for (i in 1:length(distances)) {
             d = distances[i]
             panel.curve(sqrt(d^2 - x^2), from= d * cos(3*pi/4), to=d * cos(pi/4), col=bgcol)
           }
           panel.xyplot(x,y, alpha = 0.3, ...)
         }
       , auto.key=list(columns=4)
       , xlim = c(-350, 350), ylim = c(NA, 525)
       , xlab = "Horizontal Distance from Home Plate (ft.)"
       , ylab = "Vertical Distance from Home Plate (ft.)"
  )
  return(plot)
}
