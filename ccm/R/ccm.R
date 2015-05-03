#' @title Convergent Cross Map
#' @description Run the convergent Cross Map algorithm
#' @param X first time series or vector
#' @param Y second time series or vector of the same length as X
#' @param tau stepsize of shadow_manifold
#' @param d dimension of shadow_manifold
#' @param rep_count number of slices per stretchlength in the bootstrap; if 0 it will use a complete sliding window as library
#' @param top_count number of points per slice to test in the bootstrap
#' @param N1 minimum of length of stretches
#' @param N2 maximum of length of stratches
#' @return an ccm object with parameters and results
#' @examples
#' # res <- ccm(X, Y)
#' # plot(ts(res$cor_x))
#' # lines(ts(res$cor_y))

ccm<- function (X, Y, tau = 2, d = 4, rep_count = 25, N1 = 10, N2 = 100,
                silent = FALSE, top_count=NULL)
{
  if (d >= N1) {
    cat("Parameter error: d should be strictly smaller than N1.\n")
    return(NULL)
  }
  if(N2 < N1) {
    cat("Parameter error: N2 should be >= N1.\n")
    return(NULL)
  }
  if (length(X)- N2 < (d-1)*tau){
    cat("Warning: N2 too big: adjusted to max length\n")
    N2= -1+ length(X)-(d-1)*tau
  }
  if (rep_count > 0) ### bootstrapped
    return(ccm_b(X, Y, tau = tau, E = d
                 , rep_count = rep_count, N1 = N1, N2 = N2,
                 silent = silent, top_count=top_count))
  if (rep_count ==0) ### sliding window
      return(ccm_sw(X, Y, tau = tau, E = d
                   , rep_count = 0, N1 = N1, N2 = N2,
                   silent = silent))
  if (rep_count <= 0){
    cat("Parameter error: rep_count should be > 0.\n")
    return(NULL)
  }
}

ccm_b<- function (X, Y, tau = 2, E = 4, rep_count = 25, N1 = 10, N2 = 100,
                  silent = FALSE, top_count=NULL)
{
  OFF_SET = (E-1)*tau
  # top_count<- rep_count
  if(is.null(top_count)) top_count<- min(round(.5*(N2-N1), 0), rep_count)
  x_hits <- matrix(NA, 1 + N2 - N1, rep_count)
  y_hits <- matrix(NA, 1 + N2 - N1, rep_count)
  ds_x <- shadow_manifold(X, tau, E)
  ds_y <- shadow_manifold(Y, tau, E)
  max_range <- 1 + N2 - N1
  max_l <- dim(ds_x)[1]
  cor_x <- numeric(max_range)
  cor_y <- numeric(max_range)
  for (L in 1:max_range) {
    win_len <- N1 - 1 + L
    if (!silent && L%%10 == 1) {
      cat("length ")
      cat(win_len)
      cat("\n")
    }
    pred_x <- numeric(top_count)
    pred_y <- numeric(top_count)
    actual_x <- numeric(top_count)
    actual_y <- numeric(top_count)
    cor_x_tmp <- numeric(rep_count)
    cor_y_tmp <- numeric(rep_count)
    slice_sample=  sample(1:(max_l - win_len - 1), rep_count, replace=TRUE)
    for (j in 1:rep_count) {
      slice_index=slice_sample[j]
      top_sample<-sample(1:max_l, top_count, replace=TRUE)
      for (i in 1:top_count) {
        top_index <- top_sample[i]
        window <- c(slice_index + (0:(win_len - 1)), top_index)
        ds_tmp_x <- ds_x[top_index, window]
        ds_tmp_y <- ds_y[top_index, window]
        nn_tmp_x <- nearest_neighbours(E, ds_tmp_x)
        nn_tmp_y <- nearest_neighbours(E, ds_tmp_y)
        ww <- nn_weights(dists = ds_tmp_x, nn = nn_tmp_x)
        pred_y[i] <- predict_y(Y[window+OFF_SET], wt = ww)
        actual_y[i] <- Y[top_index+OFF_SET]
        ww <- nn_weights(dists = ds_tmp_y, nn = nn_tmp_y)
        pred_x[i] <- predict_y(X[window+OFF_SET], wt = ww)
        actual_x[i] <- X[top_index+OFF_SET ]
      }
      ax <- cor(pred_x,actual_x)
      ay <- cor(pred_y, actual_y)
      cor_x_tmp[j] <- ax
      cor_y_tmp[j] <- ay
    }
    x_hits[L, ] <- cor_x_tmp
    y_hits[L, ] <- cor_y_tmp
    cor_x[L] <- median(cor_x_tmp)
    cor_y[L] <- median(cor_y_tmp)
  }
  #Truncate at 0
  cor_x[cor_x<0] <- 0
  cor_y[cor_y<0] <- 0
  param=c(tau=tau, d=E, N1=N1, N2=N2, rep_count=rep_count, top_count=top_count)
  res<-list(param=param, cor_x = cor_x, cor_y = cor_y, x_hits=x_hits, y_hits=y_hits)
  class(res)="cmm"
  return(res)
}

ccm_sw<- function (X, Y, tau = 2, E = 4, rep_count = 0, N1 = 10, N2 = 100,
                  silent = FALSE)
{
  OFF_SET = (E-1)*tau
  ds_x <- shadow_manifold(X, tau, E)
  ds_y <- shadow_manifold(Y, tau, E)
  max_range <- 1 + N2 - N1
  max_l <- dim(ds_x)[1]
  x_hits <- matrix(NA, 1 + N2 - N1, max_l)
  y_hits <- matrix(NA, 1 + N2 - N1, max_l)
  cor_x <- numeric(max_range)
  cor_y <- numeric(max_range)
  for (L in 1:max_range) {
    win_len <- N1 - 1 + L
    if (!silent) {
      cat("length ")
      cat(win_len)
      cat("\n")
    }
    top_count=max_l
    pred_x <- numeric(top_count)
    pred_y <- numeric(top_count)
    actual_x <- numeric(top_count)
    actual_y <- numeric(top_count)
    cor_x_tmp <- numeric(max_l)
    cor_y_tmp <- numeric(max_l)
    for (slice_index in 1:(max_l - win_len - 1)) {
      top_sample<-1:max_l
      for (i in 1:top_count) {
        top_index <- top_sample[i]
        window <- c(slice_index + (0:(win_len - 1)), top_index)
        ds_tmp_x <- ds_x[top_index, window]
        ds_tmp_y <- ds_y[top_index, window]
        nn_tmp_x <- nearest_neighbours(E, ds_tmp_x)
        nn_tmp_y <- nearest_neighbours(E, ds_tmp_y)
        ww <- nn_weights(dists = ds_tmp_x, nn = nn_tmp_x)
        pred_y[i] <- predict_y(Y[window+OFF_SET], wt = ww)
        actual_y[i] <- Y[top_index+OFF_SET]
        ww <- nn_weights(dists = ds_tmp_y, nn = nn_tmp_y)
        pred_x[i] <- predict_y(X[window+OFF_SET], wt = ww)
        actual_x[i] <- X[top_index+OFF_SET ]
      }
      ax <- cor(pred_x,actual_x)
      ay <- cor(pred_y, actual_y)
      cor_x_tmp[slice_index] <- ax
      cor_y_tmp[slice_index] <- ay
    }
    x_hits[L, ] <- cor_x_tmp
    y_hits[L, ] <- cor_y_tmp
    cor_x[L] <- median(cor_x_tmp)
    cor_y[L] <- median(cor_y_tmp)
  }
  #Truncate at 0
  cor_x[cor_x<0] <- 0
  cor_y[cor_y<0] <- 0
  param=c(tau=tau, d=E, N1=N1, N2=N2, rep_count=0, top_count=0)
  res<-list(param=param, cor_x = cor_x, cor_y = cor_y, x_hits=x_hits, y_hits=y_hits)
  class(res)="cmm"
  return(res)

}

#' @title Plot Convergent Cross Map
#' @description Run the convergent Cross Map algorithm and makes a plot_
#' @param X first time series or vector
#' @param Y second time series or vector of the same length as X
#' @param tau stepsize of shadow_manifold
#' @param d dimension of shadow_manifold
#' @param rep_count number of elements in the bootstrap
#' @param N1 minimum of length of stretches
#' @param N2 maximum of length of stratches
#' @param silent no pprogress reporting from ccm
#' @param tsName1 name of first vector (for plot)
#' @param tsName2 idem
#' @param all plot all intermediate results
#' @param res if a ccm object, skip the call to ccm. Equivalent to a call plot(res).
#' @return a ccm object
#' @examples
#' # res <- plot_ccm(X, Y)
#' # plot(res)

plot_ccm <-
  function(X, Y, tau = 2, d = 4, rep_count = 25, N1 = 10, N2 = 150,
           silent = FALSE, tsName1="", tsName2="", all=TRUE, res=NULL, top_count=NULL){
    chck<-require(myLib)
    if (!chck){
      cat("cannot plot, required myLib not present\n")
      return(NULL)
    }
    if (is.null(res)) res<-ccm(X, Y, tau=tau, d=d, rep_count=rep_count, N1=N1, N2=N2, silent=silent, top_count=top_count)

    # back with params
    tau = res$param['tau']
    d= res$param['d']
    rep_count= res$param['rep_count']
    top_count=res$param['top_count']
    N1= res$param['N1']
    N2= res$param['N2']
    # print(length(res$x_hits))
    N3=dim(res$x_hits)[1]
    N4=dim(res$x_hits)[2]

    #
    pretty_init(pal.nr=4)
    t2= min(round(.5*(N2-N1), 0), rep_count)
    if (!is.null(top_count)) t2=top_count
    if (rep_count != 0){
      main.string=sprintf("%s vs %s\ntau= %d, d= %d; nr of samples= %d * %d / slice", tsName1, tsName2, tau, d, rep_count, t2)
    } else {
      main.string=sprintf("%s vs %s\ntau= %d, d= %d; full sliding window", tsName1, tsName2, tau, d)
    }
    l1.string=sprintf("%s | %s", tsName2, tsName1)
    l2.string=sprintf("%s | %s", tsName1, tsName2)

    pretty_plot(ts(res$cor_x, start=N1), ylim=c(0, 1.1)
                , lwd=2             , ylab="", xlab="", kleur=2, xat=c(10, seq(20,400,20))
                , main=main.string
                , ccloc=0)
    if (all) {
      if(rep_count == 0) {
        for (L in 1:N3){
          pretty_plot(add=T, type="p", kleur=5, data.frame(x=rep(L+N1-1, N4-N1-L), res$x_hits[L,1:(N4-N1-L)]), transparent=TRUE)
        }

        for (L in 1:N3){
          pretty_plot(add=T, type="p", kleur=10, data.frame(x=rep(L+N1-1, N4-N1-L), res$y_hits[L,1:(N4-N1-L)]), transparent=TRUE)
        }

      } else {
        for (L in 1:N3){
          pretty_plot(add=T, type="p", kleur=5, data.frame(x=rep(L+N1-1, N4), res$x_hits[L,]), transparent=TRUE)
        }

        for (L in 1:N3){
          pretty_plot(add=T, type="p", kleur=10, data.frame(x=rep(L+N1-1, N4), res$y_hits[L,]), transparent=TRUE)
        }
      }
    }
    pretty_plot(ts(res$cor_x, start=N1),add=T, lwd=2, kleur=2)
    pretty_plot(ts(res$cor_y, start=N1),add=T, lwd=2, kleur=12)

    pretty_legend(kleur = c(2,13), legend=c(l1.string, l2.string), lwd=4)
    return(res)
  }

plot.cmm<-function(res , tsName1="X", tsName2="Y"){
  junk=plot_ccm( res=res
                 , tau= res$param["tau"]
                 , d= res$param["d"]
                 , N1= res$param["N1"]
                 , N2= res$param["N2"]
                 , rep_count= res$param["rep_count"]
                 , top_count= res$param["top_count"]
                 , tsName1= tsName1, tsName2=tsName2

    )
}
