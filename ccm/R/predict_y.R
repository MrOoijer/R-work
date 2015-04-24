#' @title Predict y
#' @description Predicts the value of an observation in series Y given the X-weights and nearest neighbours
#' @param sm.Y a shadow manifold
#' @param wt a vector of weights on which to calculate a spatial average
#' @return the predicted value of y (all coordinates)
#' @keywords predict
#' @seealso \code{\link{nn_weights}} 
#' @examples
#' #none


# predict_y <-
#   function(sm.Y,wt){
#     # Predict y from the weight of x
#     #   y is a point in the shadow Manifold Y
#     #   wt are nn-weights of the corresponding x in X
#     #   the prediction is the x-weighted average of the points that x thinks are close to y
#     #
#     sum(wt$w * sm.Y[wt$nn, 1]) # this is just a point sum for first coordinate
#   }

predict_y <-
  function(sm.Y,wt){
    # Predict y with full coordinates
    #
    d<-length(wt$w)-1
    sapply(1:d, function(i){sum(wt$w * sm.Y[wt$nn,i])}) # this is a vector
  }
