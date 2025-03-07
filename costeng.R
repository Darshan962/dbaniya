# set the number of significant figures for the output
options(digits=3, show.signif.stars=F)
# import the data
cost<-c(4.6, 6.2, 5.0, 6.6,   4.9, 6.3, 5.4, 6.8,    4.4, 5.9, 5.4, 6.3)
job<-c(rep(1:4,3))
eng<-c(rep(1,4),rep(2,4),rep(3,4))
cbind(eng,job,cost)

#declare factors
eng<-factor(eng)
job<-factor(job)
mod<-lm(cost~job+eng)
anova(mod)
summary(mod)

tapply(cost,eng,mean) # mean cost by engineer
qt(0.025, df=6) # critical t-value on 6 df for 95% CI