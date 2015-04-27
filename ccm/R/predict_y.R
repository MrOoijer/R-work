#' @title Predict y
#' @description Predicts the value of an observation in series Y given the X-weights and nearest neighbours
#' @param Yw the points on the Y-vector corresponding to a slice
#' @param wt a vector of weights on which to calculate a spatial average
#' @return the predicted value for y
#' @keywords predict
#' @seealso \code{\link{nn_weights}}
#' @examples
#' #none

predict_y <-
  function(Yw, wt){
    # Predict y with single coordinate
    sum(wt$w * Yw[wt$nn])
  }
