#' @title Calculate weights
#' @description Calculate "spatial" weights for each of the (d+1) nearest neighbours
#' @param m a matrix containing a shadow manifold // we don't need it here yet
#' @param dists a matrix of euclidian distances between all points on the manifold x
#' @param index containing the index of the point in m .
#' @param nn a vector of indicies of the nearest neighbours
#' @return list of weights and indices
#' @keywords weights
#' @seealso \code{\link{get_nn}} 
#' @examples
#' #none

nn_weights <-
  function(m,dists,index,nn){
    u <- exp(-dists[index,nn[index,]]/dists[index,nn[index,1]]) 
        # why divide at all? other implementation does not use it
    if(is.nan(u) || is.na(u)) { u<-rep(0, length(u)); u[1]=1} 
        # above line captures boundary case that dists(nn[2] , nn[1]) == 0; yes that happens
    w <- u / sum(u) # normalized
    return(list(w=w,nn=nn[index,]))
  }
