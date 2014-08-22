#' @title fieldingplot
#' 
#' @description Plot the fielding model for an individual player
#' 
#' @details This is a convenience function for visually checking the fielding models.
#' 
#' @param data a GameDayPlays dataset
#' @param model a model object that has a `predict` method
#' @param write.pdf a LOGICAL indicating whether to write the contourplot to a file
#' 
#' @return A contourplot object
#' 
#' @examples
#' 
#' data(MLBAM2013)
#' # not run
#' 
#' 

panel.fielding = function (x, y, z, ...) {
  panel.baseball()
  panel.contourplot(x, y, z, ...)
}


fieldingplot = function (x, data,...) UseMethod("fieldingplot")

fieldingplot.glm = function (x, data, ...) {
  fieldingplot.lm(x, data, ...)
}

fieldingplot.lm = function (x, data, ...) {
  model = x
  # make sure that the model object passed has a predict() method
  if (sum(paste("predict.", class(model), sep="") %in% methods(predict)) == 0) {
    stop(paste("There are no predict methods for model object of type", class(model)))
  }
  
  xs = seq(from = -350, to = 350, by = 10)
  ys = seq(from = -100, to = 550, by = 10)
  my.grid = expand.grid(our.x = xs, our.y = ys)
  my.grid$z.hat = predict(model, newdata=my.grid, type = "response")
  
  label = gsub("[^A-Z]", "", paste(lhs(terms(model)), collapse=""))
  
  fieldingplot.formula(z.hat ~ our.x + our.y, data=my.grid, label=label, ...)
}

fieldingplot.formula = function (x, data, label = "label", write.pdf = FALSE, ...) {
  require(RColorBrewer)
  
  if (write.pdf) {
    filename = paste("fielding_", label, ".pdf", sep="")
    pdf(file = filename, width=10, height=8)
  }
  print(contourplot(x, data=data, region=TRUE, ...
                    , alpha.regions = 0.5
                    , col.regions = colorRampPalette(brewer.pal(9, "Blues"))(100)
                    , cuts = 10, contour=TRUE
                    , panel = panel.fielding
#                    , xlim = c(-350, 350), ylim = c(0, 550)       
                    , xlab = "Horizontal Distance from Home Plate (ft.)"
                    , ylab = "Vertical Distance from Home Plate (ft.)"
  ))
  if (write.pdf) {
    dev.off()
  }
}

fieldingplot.default = function (x, y, z, label = "label", write.pdf = FALSE, ...) {
  stop("No available methods")
}



