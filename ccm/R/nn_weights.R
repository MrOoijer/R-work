#' @title Calculate weights
#' @description Calculate "spatial" weights for each of the (E+1) nearest neighbours
#' @param dists a matrix of euclidian distances between all points on the manifold x
#' @param nn a vector of indicies of the nearest neighbours
#' @return list of weights and indices
#' @keywords weights
#' @examples
#' #none

nn_weights <-
  function(dists,nn){
    u <- exp(-dists[nn]/dists[nn[1]])
    # why divide at all? other implementation does not use it
    if(is.nan(u) || is.na(u)) { u<-rep(0, length(u)); u[1]=1}
    # above line captures boundary case that dists(nn[2] , nn[1]) == 0; yes that happens
    w <- u / sum(u) # normalized
    return(list(w=w,nn=nn))
  }
