#' @title preRS
#' @description calculate predictive R-squared aka leave one out Cross Validation
#' @param this.lm a linear model object
#' @return the preRS statistic - please note this only works for a linear model
#' @examples
#' # preRS(lm)
#' ## 0.78
#' # hc4.ts <- rd.hc4()
#' # this.lm <- lm(hc4.ts~index(hc4.ts))
#' # print(summary(this.lm))
#' # print(preRS(this.lm))

preRS <- function(this.lm){
  this.anova <- anova(this.lm)
  tss <- sum(this.anova$"Sum Sq")
  pr <- residuals(this.lm)/(1 - lm.influence(this.lm)$hat)
  PRESS <- sum(pr^2)
  return(1-PRESS/tss)
}

# test
# read.hc4()
# this.lm<-lm(hc4.ts~index(hc4.ts))
# print(summary(this.lm))
# print(preRS(this.lm))