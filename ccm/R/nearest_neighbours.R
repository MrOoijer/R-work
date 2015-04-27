#' @title Get nearest neighbours
#' @description Finds the indices of the nearest neighbours to a point on a manifold
#' @param E the dimension of the shadow manifold
#' @param dists a one dimensional slices of distances
#' @return vector of indices of the E+1 nearest neighbours
#' @keywords neighbours
#' @seealso \code{\link{get_nn}}
#' @examples
#' # none

nearest_neighbours <-
  function(E, dists){
    # dists is a one dimensional slices of distances
    nn <- order(dists)
    nn <- nn[1:(E+1)]
    return(nn) # so this returns place in the original slice
  }
