#' @title pretty_cc
#' @description called from pretty_plot, do not use otherwise

pretty_cc <- function(ccloc=0){
  if (ccloc %in% c(1,3)){
    loc= ccloc
    l = -1.6
    a = 0.95
  } else {
    loc= ccloc-1
    l = -1.6
    a = 0.05
  }
  if (ccloc == 0) return
  mtext("CC Jan van Rongen 2015"
        , col=cpalet$as
        , side=loc, line=l, adj=a, cex=0.65)
}
