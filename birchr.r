options(digits=3, show.signif.stars=F)
birchdf<-read.table("birch.txt",header=T)
attach(birchdf)
interaction.plot(Species, Treat, ATP)
birch.lm<-lm(ATP~Species*Treat)
anova(birch.lm)
summary(birch.lm)
confint(birch.lm)
attach(birchdf)
tapply(ATP, Species, mean)
tapply(ATP,Treat, mean)
tapply(ATP, list(Treat, Species), mean)

#predict means and se's from model
 pred.frame<-data.frame(species=c("EC","EF","RC","RF"))
 pred<-predict(birchlm,newdata=pred.frame,se=T)
 pred.frame$Yhat<-pred$fit
 pred.frame$se<-pred$se.fit
#calculate CIs
 pred.frame$ll<-pred.frame$Yhat-2*pred.frame$se
 pred.frame$ul<-pred.frame$Yhat+2*pred.frame$se
 attach(pred.frame)
#plot the CIs
 dotchart(Yhat,xlim=c(min(ll),max(ul)))
 mnlines<-1:4
 points(ll,mnlines,pch="[")
 points(ul,mnlines,pch="]")
# using confint to determine CIs
 birch2.lm <- birchlm<-lm(atp~species-1,data=birchdf)
 ci<-confint(birch2.lm)
#plot the CIs
 dotchart(Yhat,xlim=c(min(ci[,1]),max(ci[,2])))
 legend(0.1,4.9,legend=c("1=European (Control)","2=European (Flooded)","3=River (Control)","4=River (Flooded)"))
 mnlines<-1:4
 points(ci[,1],mnlines,pch="[")
 points(ci[,2],mnlines,pch="]")


