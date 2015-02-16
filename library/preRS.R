# predictive sum of squares
#
# in: lineair model my.lm
#
# out: predictive R2 scalar
#

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