x1<-sample(1:3, 100, replace = TRUE)
x2<-sample(0:2, 100, replace = TRUE)
y<- 1 + 2*x1 - 1*x2 - 2*x1*x2
new<-data.frame(x1,x2,y)
with(new, interaction.plot(x1,x2,y))

energy<-read.csv("Cereal.csv")
library(GGally)
ggpairs(energy)
mod1<-lm(Energy~Protein+Fat+Fibre+Carbs, data=energy)
summary(mod1)
mod2<-lm(Energy~Protein*Fat*Fibre*Carbs, data=energy)
mod2<-lm(Energy~Protein+Fat+Fibre+Carbs+Fibre:Carbs, data=energy)
mod2<-lm(Energy~(Protein+Fat+Fibre+Carbs)^2, data=energy)
summary(mod2)

mod3<-lm(Energy~Fat+Fibre+Carbs+Fat:Carbs+Fibre:Carbs, data=energy)
summary(mod3)
mod4<-lm(Energy~Fibre+Carbs+Fibre:Carbs, data=energy)
summary(mod4)

plot(mod4, which=1:2)

Fibre<-c(7,7)
Carbs<-c(10, 30)
new2<-data.frame(Fibre,Carbs)
predict(mod4, new2, interval="confidence", level=0.95)

ggplot(aes(x=Fibre, y=Carbs), data = energy)+
  geom_point()+
  geom_point(aes(x=7, y=10), colour="green")+
  geom_point(aes(x=7, y=30), colour="red")
