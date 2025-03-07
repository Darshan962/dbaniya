options(digits=3, show.signif.stars=F)
tulips<-read.table("tulips.txt",header=T)

tulips$BED<-factor(tulips$BED)
tulips$WATER<-factor(tulips$WATER)
tulips$SHADE<-factor(tulips$SHADE)

tulip.lm<-lm(BLOOMS~BED+WATER*SHADE,data=tulips)
library(effects)
tuleff<-allEffects(tulip.lm)
plot(tuleff, layout = c(3,1))
print(tuleff)
summary(tulip.lm)
anova(tulip.lm)
attach(tulips)
interaction.plot(WATER,SHADE,BLOOMS)

# Contrasts for shade
t1<-C(tulips$SHADE,c(2,-1,1),1)
t2<-C(tulips$SHADE,c(0,1,-1),1)

tulip2aov<-lm(BLOOMS~BED+WATER +t1 + t2 + WATER:SHADE,data=tulips,x=T)
summary(tulip2aov)
anova(tulip2aov)
