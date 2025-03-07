#################################################################
#			prac5 STAT210/410			
#				GLMs				
#################################################################

####################Question 2 - UCLA admissions#################

options(digits=3,show.signif.stars=F) 

# Access the dataset from a web location
UCLAadm <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

# View the first six rows of your dataframe so you know what it looks like.
head(...)

# One of the qualitative predictors needs to be declared as a factor
# Convert that variable  to a factor.  
# In the code below you need to replace ... with the name of your dataframe.
# and replace xxx with the name of the variable
...$xxx <- factor(...$xxx)
attach(UCLAadm)


# Plot the data - in this case for gre. 
plot(gre,admit, xlab="gre",ylab="Probability of admission")
gre.glm=glm(admit~gre,family=binomial,UCLAadm)
curve(predict(gre.glm,data.frame(gre=x),type="resp"),add=TRUE)
points(gre,fitted(gre.glm),pch=20)

# Now produce a similar plot with gpa as the predictor.

..............

# Now fit a glm with gre, gpa and rank as predictors

adm.glm <- .........

# Produce the anova table of deviance

.............

# Check goodness-of-fit

............

# Produce a summary table of regression coefficients and 95% CIs

...........

# Calculate odds ratios and CIs
exp(cbind(OR = coef(adm.glm), confint(adm.glm)))


####################################################################
#### Only if you've got time (or can perhaps explore later ...)
###################################################################
# Set up a data frame containing the values you want for the independent variables
newdata <- data.frame(
  gre = rep(seq(from = 200, to = 800, length.out = 100), 4 * 4),
  gpa = rep(c(2.5, 3, 3.5, 4), each = 100 * 4),
  rank = factor(rep(rep(1:4, each = 100), 4)))

# Predict the probabilities for our input data as well as their standard errors.  
#Remember to adjust the name of the model as appropriate.
newdata[, c("p", "se")] <- predict(a, newdata,
  type = "response", se.fit=TRUE)[-3]

# Load ggplot2
library(ggplot2)
# Create four plots, one for each level of gpa we used (2.5, 3, 3.5, 4) with the colour of the lines indicating the rank the predicted probabilities were for.

ggplot(newdata, aes(x = gre, y = p, colour = rank)) +
  geom_line() +
  facet_wrap(~gpa)