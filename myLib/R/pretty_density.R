#' @title pretty_density
#' @description makes a pretty density plot with mean and CI
#' @param data a vector with the data
#' @param mid is NA, the mean of the data, otherwise a given value such as median
#' @param kleur the line color in the palet
#' @param ... other values for pretty_plot
#' @return NA
#' @examples
#' # pretty_density(rnorm(1000, 0,1), mid=0)
pretty_density <- function(data
                           , xlab="value"
                           , ylab=""
                           , main="Density plot with 95% confidence interval"
                           , kleur = 1
                           , ccloc = 0
                           , mid = NA
                           , ...){
  dens=density(data, bw="SJ")
  if (is.na(mid)) mid=mean(data)
  p=round(c(quantile(data, prob=c(0.025, 0.975)), mid),2)
  pretty_plot(data.frame(dens$x, dens$y), main=main, xlab=xlab, ylab=ylab, kleur=kleur, ccloc=ccloc, xat=p, ...)
  pretty_abline(kleur=kleur, lty=2, v=p)
}
