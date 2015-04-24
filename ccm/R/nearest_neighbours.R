#' @title Get nearest neighbours
#' @description Finds the indicies of the nearest neighbours to a point on a manifold
#' @param m a shadow manifold
#' @param dists a matrix of euclidian distances between all points on the manifold x
#' @return vector of indices of the E+1 nearest neighbours
#' @keywords neighbours
#' @seealso \code{\link{get_nn}}
#' @examples
#' # none

nearest_neighbours <-
  function(m, dists){
  E <- ncol(m)
  diag(dists) <- rep(1e+40, dim(dists)[1]) # ! never near to itself
  nn <- t(apply(dists,1,order))
  nn <- nn[,1:(E+1)]
  return(nn)
}
