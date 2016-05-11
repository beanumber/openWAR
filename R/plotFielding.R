#' @title Plot fielding model for a player.
#' 
#' @description Creates a plot of the fielding model for an individual player.
#' 
#' @details This is a convenience function for visually checking the fielding models. 
#' It provides a way to inspect the quality of the fielding models for individual
#' players, and was used to generate one of the figures used in the \code{\link{openWAR}}
#' paper. 
#' 
#' @param x a model object, currently \code{\link{lm}}, \code{\link{glm}}, and
#' \code{\link{bkde2D}} are
#'  supported. For the default method this is an x-coordinate to be passed to
#'  \code{\link{contourplot}}.
#' @param y y-coordinate for default method passed to \code{\link{contourplot}}.
#' @param z z-coordinate for default method passed to \code{\link{contourplot}}.
#' @param ... arguments passed to \code{\link{panel.contourplot}}
#' 
#' @import lattice
#' @importFrom RColorBrewer brewer.pal
#' @importFrom mosaic lhs
#' 
#' @export
#' @return A contourplot object

plotFielding = function(x, ...) UseMethod("plotFielding")

#' @export
#' @rdname plotFielding

plotFielding.glm = function(x, ...) {
    NextMethod()
}

#' @export
#' @importFrom mosaic lhs
#' @importFrom stats terms
#' @rdname plotFielding

plotFielding.lm = function(x, ...) {
    model = x
    # make sure that the model object passed has a predict() method
    if (sum(paste("predict.", class(model), sep = "") %in% methods(predict)) == 0) {
        stop(paste("There are no predict methods for model object of type", class(model)))
    }
    
    xs = seq(from = -350, to = 350, by = 10)
    ys = seq(from = -100, to = 550, by = 10)
    my.grid = expand.grid(our.x = xs, our.y = ys)
    my.grid$z.hat = predict(model, newdata = my.grid, type = "response")
    
    label = gsub("[^A-Z]", "", paste(mosaic::lhs(stats::terms(model)), collapse = ""))
    
    plotFielding.formula(z.hat ~ our.x + our.y, data = my.grid, label = label, ...)
}

#' @export
#' @importFrom grDevices colorRampPalette
#' @import lattice
#' @importFrom RColorBrewer brewer.pal
#' @rdname plotFielding

plotFielding.formula = function(x, ...) {
    print(lattice::contourplot(x, region = TRUE, ..., alpha.regions = 0.5, 
                      col.regions = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100)
                      , cuts = 10, contour = TRUE, panel = panel.fielding
#        , xlim = c(-350, 350), ylim = c(0, 550)
        , xlab = "Horizontal Distance from Home Plate (ft.)", 
        ylab = "Vertical Distance from Home Plate (ft.)"))
}

#' @export
#' @rdname plotFielding

plotFielding.default = function(x, ...) {
    stop("No available methods")
}


#' @export
#' @rdname plotFielding
#' @method plotFielding bkde2D
#' 
#' @examples 
#' fmod <- getModelFieldingCollective(May)
#' plotFielding(fmod)
#' 

plotFielding.bkde2D <- function(x, ...) {
  contourplot(x = x$fhat, row.values = x$x1, column.values = x$x2,
              panel = panel.fielding, region = TRUE, alpha.regions = 0.5, 
              col.regions = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100), 
              cuts = 10, contour = TRUE,
              #        , xlim = c(-350, 350), ylim = c(0, 550)
              xlab = "Horizontal Distance from Home Plate (ft.)", 
              ylab = "Vertical Distance from Home Plate (ft.)", 
              ...
              )
}


#' @rdname plotFielding
#' @export

panel.fielding = function(x, y, z, ...) {
  panel.baseball()
  panel.contourplot(x, y, z, ...)
}
