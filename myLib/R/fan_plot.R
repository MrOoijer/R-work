#' @title fan_plot
#' @description called from pretty_plot, do not use otherwise

fan_plot <- function(df
                     , lwd=1
                     , cex=0.4
                     , kleur=1
                     , pch=19
){
  d1<-dim(df)[1]
  d2<-dim(df)[2]
  a<-rep(mean(df[,2]), d1)
  if (d2 >2 ) a<-df[,3]
  pretty_plot(cbind(df[,1], a),kleur=cpalet$lijnkleur[kleur], lwd=lwd, type="l", add=TRUE)
  for (i in 1:d1){
    c = rgb(1,0,0,0.5)
    if (df[i,2]<a[i]) c = rgb(0,0,1,0.5)
    if(cex >0.1) points(df[i,c(1,2)], col=c, pch=pch, cex=cex)
    lines(rbind(c(df[i,1],df[i,2]),c(df[i,1],a[i]))
          , col=c
          , lwd=lwd)
  }
}
