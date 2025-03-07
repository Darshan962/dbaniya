options(digits=6, show.signif.stars=F)
library(lattice)
samara.df <- read.table("samara.txt",header=T)

samaraEDA <- xyplot(Velocity ~ Load|Tree,data=samara.df,layout=c(3,1),
                    panel=function(x,y){
                      panel.xyplot(x,y)
                      panel.lmline(x,y)} )
print(samaraEDA)

samara.df$Tree<-factor(samara.df$Tree,labels=c("T1","T2","T3"))
# Fit three models (interaction, main effects, simple linear regression)

# model 1: non-parallel lines
model1<-lm(Velocity~Load*Tree, data=samara.df)
#model2: parallel lines
model2<-lm(Velocity~Load+Tree, data=samara.df)
# model3: Simple linear regression
model3<-lm(Velocity~Load, data=samara.df)
#Compare the 3 models
anova(model1,model2, model3)

#stepwise variable selection.
#forwards
formL <- formula(~ 1)
formU <- formula(~ Load*Tree, data=samara.df)
start.model <- lm(Velocity ~1,data=samara.df)
stepf.model <- step(start.model,direction="forward",scope=list(lower=formL,upper=formU))
anova(stepf.model)
summary(stepf.model)
confint(stepf.model)

#backwards
formL <- formula(~ 1)
formU <- formula(~ Load*Tree, data=samara.df)
start.model <- lm(Velocity ~Load*Tree,data=samara.df)
stepb.model <- step(start.model,direction="backward",scope=list(lower=formL,upper=formU))
anova(stepb.model)
summary(stepb.model)
confint(stepb.model)


