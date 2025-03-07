options(digits= 4, show.signif.stars=F)
library(effects)

  # analyse mangold
 mangold <- read.table('mangold.txt',header=T)
 mangold$nh4 <- factor(mangold$nh4)
 mangold$salt <- factor(mangold$salt)
 mangold$dung <- factor(mangold$dung)
 mangold$block <- factor(mangold$block)
 attach(mangold)
 
# plot.design(yield ~ block + nh4 + salt + dung,data=mangold,las=1)
 mangold.lm <- lm( yield ~ block+nh4*salt*dung,data=mangold)
 anova(mangold.lm)
  plot(allEffects(mangold.lm))

 interaction.plot(nh4,salt,yield,las=1)
library(dae)
 interaction.ABC.plot(yield,nh4,salt,dung, data=mangold)

 #three-way interaction using ggplot with points
 library(ggplot2)
 p <- ggplot(mangold, aes(x=nh4, y=yield, colour=salt, shape=salt, Size=salt)) +
    geom_point(aes())
 p + facet_grid(. ~ dung)
 
 detach(mangold)
 mangold.lm <- lm( yield ~ block+nh4*salt*dung,data=mangold)
 print(anova(mangold.lm))

 interaction.plot(nh4,salt,yield, data=mangold)
 

 model<-lm(yield~block+ nh4*salt+dung,data=mangold)
 confint(model)
 with(mangold, interaction.plot(nh4, salt, yield))
 
 maneff<-effect("nh4:salt",model)
 maneff; maneff$se
 
 maneff2<-effect("dung",model)
 maneff2; maneff2$se


model<-lm(yield~block+dung+ nh4+salt+ nh4:salt,data=mangold)
confint(model)
par(mfrow=c(2,2))
    plot(model,which=1:4)
shapiro.test(model$residuals)

