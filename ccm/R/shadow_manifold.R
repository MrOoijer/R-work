#' @title Make Shadow manifold
#' @description Make Shadow manifold from observed data v with stepsize tau and dimension E.
#' @param v a a vector containing the observed data
#' @param tau the step size
#' @param E the dimension
#' @return array - the multi-dimensional vector (v(t), v(t-tau), ... v(t-(E-1)*tau))
#' @keywords shadow
#' @seealso \code{\link{ccm}}
#' @examples
#' #none
shadow_manifold<-function(v, tau=1, E=3){
  l <- length(v)
  x <- array(dim=c(l-(E-1)*tau, E))
  tmap <- (1+(E-1)*tau):l
  for(i in tmap)
    x[i-((E-1)*tau),] <- v[seq(i,(i-(E-1)*tau),-tau)]
  return(x)
}
