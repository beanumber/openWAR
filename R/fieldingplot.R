#' @title fieldingplot
#' 
#' @description Creates a plot of the fielding model for an individual player
#' 
#' @details This is a convenience function for visually checking the fielding models. 
#' It provides a way to inspect the quality of the fielding models for individual
#' players, and was used to generate one of the figures used in the \code{\link{openWAR}}
#' paper. However, this function is not exported, since it is not really accessible
#' to end users. We hope to add this functionality in a future release. 
#' 
#' @param x a model object, currently \code{\link{lm}} and \code{\link{glm}} are
#'  supported. For the default method this is an x-coordinate to be passed to
#'  \code{\link{contourplot}}.
#' @param y y-coordinate for default method passed to \code{\link{contourplot}}.
#' @param z z-coordinate for default method passed to \code{\link{contourplot}}.
#' @param data a GameDayPlays dataset
#' @param label a string to be used in the resulting filename
#' @param write.pdf a \code{logical} indicating whether to write the contourplot
#'  to a file
#' @param ... arguments passed to \code{\link{panel.contourplot}}
#' 
#' @importFrom lattice contourplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom mosaic lhs
#' 
#' @return A contourplot object
#' 
#' @examples
#' deltas <- makeWARre24(May)
#' Mayplus = cbind(May, deltas)
#' library(dplyr)
#' BIP <- filter(Mayplus, isBIP == TRUE)
#' \dontrun{
#' fielding = makeWARFielding(BIP)
#' }
#' 

fieldingplot = function(x, data, ...) UseMethod("fieldingplot")

#' @rdname fieldingplot

fieldingplot.glm = function(x, data, ...) {
    fieldingplot.lm(x, data, ...)
}

#' @rdname fieldingplot

fieldingplot.lm = function(x, data, ...) {
    model = x
    # make sure that the model object passed has a predict() method
    if (sum(paste("predict.", class(model), sep = "") %in% methods(predict)) == 0) {
        stop(paste("There are no predict methods for model object of type", class(model)))
    }
    
    xs = seq(from = -350, to = 350, by = 10)
    ys = seq(from = -100, to = 550, by = 10)
    my.grid = expand.grid(our.x = xs, our.y = ys)
    my.grid$z.hat = predict(model, newdata = my.grid, type = "response")
    
    label = gsub("[^A-Z]", "", paste(lhs(terms(model)), collapse = ""))
    
    fieldingplot.formula(z.hat ~ our.x + our.y, data = my.grid, label = label, ...)
}

#' @rdname fieldingplot

fieldingplot.formula = function(x, data, label = "label", write.pdf = FALSE, ...) {
    
    if (write.pdf) {
        filename = paste("fielding_", label, ".pdf", sep = "")
        pdf(file = filename, width = 10, height = 8)
    }
    print(contourplot(x, data = data, region = TRUE, ..., alpha.regions = 0.5, 
                      col.regions = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100)
                      , cuts = 10, contour = TRUE, panel = panel.fielding
#        , xlim = c(-350, 350), ylim = c(0, 550)
        , xlab = "Horizontal Distance from Home Plate (ft.)", 
        ylab = "Vertical Distance from Home Plate (ft.)"))
    if (write.pdf) {
        dev.off()
    }
}

#' @rdname fieldingplot

fieldingplot.default = function(x, y, z, label = "label", write.pdf = FALSE, ...) {
    stop("No available methods")
}

#' @rdname fieldingplot
#' @importFrom lattice panel.contourplot

panel.fielding = function(x, y, z, ...) {
  panel.baseball()
  panel.contourplot(x, y, z, ...)
}
