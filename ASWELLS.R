options(digits=3, show.signif.stars=FALSE)
# read in data and store in object named ASWELLS.df
ASWELLS.df<-read.table("CH04_ASWELLS.txt",header=T)
attach(ASWELLS.df)
# Pairs  plot with the response variable ARSENIC in bottom right hand corner
pairs(ASWELLS.df,lower.panel=panel.smooth, upper.panel=panel.cor)
# Fit linear regression model using lm() function call
# lm stands for linear model
mod1<-lm(ARSENIC~LATITUDE+LONGITUDE+DEPTHFT, data = ASWELLS.df)
# table of paranmeter estimates, and t-tests
summary(mod1)
# 95% CIs for parameters 
confint(mod1)
# anova table, including MSE, estimate of sigma^2
anova(mod1)

mod2<-lm(ARSENIC~(LATITUDE+LONGITUDE)*DEPTHFT, data = ASWELLS.df)
summary(mod2)
anova(mod2)
plot(mod2, which=1:2)

# residuals plots to check model assumptions
par(mfrow=c(1,2))
plot(mod1, which=1:2)

boxcox(ARSENIC~LATITUDE+LONGITUDE+DEPTHFT,lambda=seq(0,0.5,0.01))
modt<-lm(ARSENIC^0.2~LATITUDE+LONGITUDE+DEPTHFT, data = ASWELLS.df)
# table of paranmeter estimates, and t-tests
summary(modt)
# 95% CIs for parameters 
confint(modt)
# anova table, including MSE, estimate of sigma^2
anova(modt)
# residuals plots to check model assumptions
par(mfrow=c(1,2))
plot(modt, which=1:2)


# predictions from the model
attach(ASWELLS.df)
LONGITUDE<-90.67;LATITUDE<- 23.74;DEPTHFT<-210
newX<-data.frame(LONGITUDE,LATITUDE,DEPTHFT )
predict(mod1,newdata=newX, interval="confidence")
predict(mod1,newdata=newX, interval="prediction")



