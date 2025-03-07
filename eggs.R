options(digits=2)
library(lattice)

egg <- expand.grid(block=1:4,light=c("O","E","F"))
egg$eggs <- c(330, 288, 295, 313,   372, 340, 343, 341,    359, 337, 373, 302)
egg$block <- factor(egg$block)

# CRD analysis (incorrect)
crd.model <- lm(eggs ~ light,data=egg)
anova(crd.model)

# randomised block design
rcb.model<-lm(eggs~block+light,
           data=egg)
print(anova(rcb.model))
print(summary(rcb.model))

# Regression coefficients and 95% CI
confint(crd.model)
confint(rcb.model)

