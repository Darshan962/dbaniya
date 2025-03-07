options(digits=2)
library(lattice)

melons <- read.table("melons.txt",header=T)
attach(melons)
dp <- densityplot(~ yield|Variety,data=melons,layout=c(1,4) )
print(dp)
# fit yield~variety
 melon.model <- lm(yield ~ Variety,data=melons)
 print(summary(melon.model))
 
# Define contrasts
Cmat <- matrix(c(1,-1,1,-1,  1,0,-1,0,  0,1,0,-1),nrow=4,ncol=3,
   dimnames=list(NULL,c("A,C v B,D"," A v C","B v D") ) )
contrasts(melons$Variety) <- Cmat

# label contrasts
melons$ACvBD <- C(melons$Variety,Cmat[,1],1)
melons$AvC <- C(melons$Variety,Cmat[,2],1)
melons$BvD <- C(melons$Variety,Cmat[,3],1)

# fit model with contrasts
melon.model3 <- lm(yield ~  AvC + BvD +ACvBD ,data=melons)
 print(anova(melon.model3))
confint(melon.model3)


