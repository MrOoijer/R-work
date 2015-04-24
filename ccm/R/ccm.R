#' @title Convergent Cross Map
#' @description Run the convergent Cross Map algorithm
#' @param X first time series or vector
#' @param Y second time series or vector of the same length as X
#' @param tau stepsize of shadow_manifold
#' @param d dimension of shadow_manifold
#' @param rep_count number of elements in the bootstrap
#' @param N1 minimum of length of stretches
#' @param N2 maximum of length of stratches
#' @return the two correlation vectors in a list
#' @keywords neighbours
#' @seealso \code{\link{get_nn}}
#' @examples
#' # res <- ccm(X, Y)
#' # plot(ts(res$cor_x))
#' # lines(ts(res$cor_y))

ccm <- function (X, Y, tau=2, d=4, rep_count=25, N1=10, N2=150){
  library(energy) # for dcor!

  # calculates the predictability of X|Y and Y|X.

  # (1) prepare data structures
  #
  x_hits <<- matrix(NA, 1+N2-N1, rep_count) # global matrices to store all results
  y_hits <<- matrix(NA, 1+N2-N1, rep_count) # ... for debugging or reporting
  sm_x <- shadow_manifold(X, tau, d)        # shadow manifolds
  sm_y <- shadow_manifold(Y, tau, d)
  ds_x <- as.matrix(dist(sm_x))             # distance matrices
  ds_y <- as.matrix(dist(sm_y))
  nn_x <- nearest_neighbours(sm_x,ds_x)     # all d+1 nearest neighbours
  nn_y <- nearest_neighbours(sm_y,ds_y)
  max_range <- 1+N2-N1                      # maximum number of points to consider
  max_l <- dim(ds_x)[1]                     # X and Y are assumed to be same length!
  cor_x <- numeric(max_range)               # place holders for results
  cor_y <- numeric(max_range)

  # (2) for each Stretch length we get rep_count random samples from X and Y to test
  for(L in 1:max_range){
    win_len <- N1-1+L                       # start with window length of 10, ending with 150
    pred_x <- matrix(nrow=win_len, ncol=d)              # place holders for intermediary results
    pred_y <- matrix(nrow=win_len, ncol=d)
    actual_x <- matrix(nrow=win_len, ncol=d)
    actual_y <- matrix(nrow=win_len, ncol=d)
    cor_x_tmp <- numeric(rep_count)
    cor_y_tmp <- numeric(rep_count)

    for(j in 1:rep_count){
      t_index<-sample(1:(max_l-win_len-1), 1)
      window <- t_index+ (0:(win_len-1))
      ds_tmp_x <- ds_x[window,window]       # more place holders
      ds_tmp_y <- ds_y[window,window]
      x=sm_x[window,]
      y=sm_y[window,]
      nn_tmp_x <- nearest_neighbours(x,ds_tmp_x)
      nn_tmp_y <- nearest_neighbours(y,ds_tmp_y)

      # (3) for each point pair in the random sample get the predictions
      for(i in 1:win_len){
        ww <- nn_weights(m=x,dists=ds_tmp_x,index=i,nn=nn_tmp_x)
        pred_y[i,] <- predict_y(sm.Y=y,wt=ww)
        actual_y[i,] <- y[i,]
        ww <- nn_weights(m=y,dists=ds_tmp_y,index=i,nn=nn_tmp_y)
        pred_x[i,] <- predict_y(sm.Y=x,wt=ww)
        actual_x[i,] <- x[i,]
      }

      # (4) for result of each L-sized random sample, calculate correlation
      # this has now become become a distance correlation - dcor in package energy
      ax <- dcor(pred_x,actual_x, index=2)
      # if(is.na(ax))ax=0                     # only for cor
      ay <- dcor(pred_y,actual_y, index=2)
      # if(is.na(ay)) ay=0
      cor_x_tmp[j]<-ax;  cor_y_tmp[j]<-ay
    }

    x_hits[L,]<<-cor_x_tmp
    y_hits[L,]<<-cor_y_tmp
    cor_x[L] <- mean(cor_x_tmp)
    cor_y[L] <- mean(cor_y_tmp)
  }
  #   cor_x[cor_x<0] <- 0                      #Truncate at 0
  #   cor_y[cor_y<0] <- 0                     # not needede when dcor, because that is 0<dcor<1
  #
  return(list(cor_x=cor_x,cor_y=cor_y))
}
