#################################################################
#			prac5 STAT210/410			
#				GLMs				
#################################################################


####################Question 1 - Coral bleaching#################

options(digits=3,show.signif.stars=F) 
 

# read in the data 

coral.df <- read.table("coral.txt",header=TRUE)


# Plot the data - produce an exploratory plot.  
attach(coral.df)
plot(temperature,bleach,xlab="Water temperature",ylab="Probability of bleaching event")

# Run the GLM.  
# Notice how the R function used is glm 
# but that the statement of the model y~x is the same format as for lm
# with response variable, bleach and the explanatory variable,temperature.  
# In addition you need to state the error distribution 
# In this case: family=binomial.

mod.glm <- glm(bleach~temperature,family=binomial,coral.df)

# We then test for model goodness of fit by first calculating:

anova(..., test="Chisq")


# And then inserting the correct values for (i) residual deviance and (ii) df into the R command below.  
# You'll need to find these missing values in the output from the previous command.
1 - pchisq(RESIDUAL DEVIANCE, df = ..)

# Now let's look at the regression coefficients
summary(...)

# And find the confidence intervals for these coefficients
confint(...)

# Predict the probability of coral bleaching when temperature = 30 C
predict(..., newdata = data.frame(temperature = 30), type="response")

# Superimpose the fitted curve (predicted values from the glm model, mod.glm) on the plot of the observed data.
# Note the use of type="response"

# Add smooth curve
curve(predict(mod.glm,data.frame(temperature=x),type="response"),add=TRUE)
# add predicted (fitted) values
points(temperature,mod.glm$fitted,pch=20)


#Done ... with Question 1.



