#' @title Make Shadow manifold
#' @description Make distance matrix for Shadow manifold of vector v
#' @param v a a vector containing the observed data
#' @param tau the step size
#' @param E the dimension
#' @return the distance matrix of the shadow manifold
#' @keywords shadow
#' @seealso \code{\link{ccm}}
#' @examples
#' #none
shadow_manifold <-
  function(v, tau=1, E=4){
    # create a shadow_manifold x from v, and for that the distance matrix
    # don't need the shadow anywhere else
    # so leave it here
    # use offset (E-1)*tau in main routine, which is the offset from v to x
    l <- length(v)
    x <- array(dim=c(l-(E-1)*tau, E))
    tmap <- (1+(E-1)*tau):l
    for(i in tmap)
      x[i-((E-1)*tau),] <- v[seq(i,(i-(E-1)*tau),-tau)]
    ds_x <- as.matrix(dist(x))
    diag(ds_x) <- rep(1e+40, dim(ds_x)[1])
    return(ds_x)
  }
