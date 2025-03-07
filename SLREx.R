
options(digits=3,show.signif.stars=F)
# read in data and attach dataframe to search path
dat1 <- read.table("CH03_SLREx.txt",header=T)
attach(dat1)

# fit simple linear regression model
xy.lm <- lm(ODP~SRP, data=dat1)

#plot data and fitted line
plot(ODP~SRP,data=dat1, ylab = "oxygen demand percent", xlab="solids reduction percent")
abline(xy.lm)

# Table of coefficients and aov table
print(summary(xy.lm))
print(anova(xy.lm))

#CI for regression coefficients
confint(xy.lm)

#diagnostic plots
par(mfrow=c(1,2))
plot(xy.lm,which=1:2, add.smooth=F)


#Shapiro Wilk's Test of normality
print(shapiro.test(xy.lm$residuals))

# predictions for 8 data points: SRP = 10,20,30,40,50,60,70,80
# create new data frame of 8 observations
 detach(dat1)

pred.df <- data.frame(SRP=c(10,20,30,40,50,60,70,80))
CI <- predict(xy.lm,interval="confidence",newdata=pred.df,level=0.95)
PI<- predict(xy.lm,interval="predict",newdata=pred.df,level=0.95)
attach(pred.df)
cbind(SRP,CI,PI)
par(mfrow=c(1,1))
 plot(pred.df$SRP,CI[,1],type="b", pch=16, xlab="SRP", ylab="mean ODP", main="Scatterplot of ODP~SRP, with predicted values and 95% Confidence and Prediction Bands")
 points(dat1$ODP, dat1$SRP)
 legend(10,75, lty=c(1, 2,3),  legend=c("Line of best fit", "95% Confidence Bands", "95% Prediction Bands"))
 legend(10, 62, pch=c(1,16), legend = c("observed values", "predicted values"))
 lines(pred.df$SRP,CI[,2],lty=2)
 lines(pred.df$SRP,CI[,3],lty=2)
 lines(pred.df$SRP,PI[,2],lty=3)
 lines(pred.df$SRP,PI[,3],lty=3)



