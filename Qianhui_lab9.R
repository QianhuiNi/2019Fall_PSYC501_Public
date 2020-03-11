library(psych)
lab9b <- read.csv(file.choose(),header=T,)
head(lab9b)
#(1)
lm <- lm(lab9b$DIST ~ lab9b$MPH )  
lm  
summary(lm)

#2.1 Check linearity:
#2.1.1 Using the raw data:
plot(x = lab9b$MPH , y = lab9b$DIST , main = "Moving speeds vs. Stopping distances")
abline(lm , col = "blue") 
lines(lowess(x = lab9b$MPH , lab9b$DIST) , col = "red")

#2.1.2 Using the residuals and predicted values:
MPHDIS_res <- residuals(lm)     
MPHDIS_pred <- predict(lm)

#Plot the residuals against the predicted values:    we expect no relationship (i.e., flat line)
plot(x = MPHDIS_pred , y = MPHDIS_res , main = "Predicted values vs. Residuals" , pch = 3)
abline(h = 0 , col = "blue" )
lines(lowess(x = MPHDIS_pred , y = MPHDIS_res) , col = "red")

#2.2 Check homoscedasticity:
#Use the residual scatterplot we just created.
#equal variability across distribution?

#2.3 Check normality:
#2.3.1 Use describe() to check the mean and median as well as skewness and kurtosis.
describe(MPHDIS_res)

#2.3.2 Use multi.hist(), boxplot(), and qqnorm() 
multi.hist(MPHDIS_res)   #gives histogram, density plot, and normal curve
boxplot(MPHDIS_res)

qqnorm(MPHDIS_res)   
qqline(MPHDIS_res)

#2.3.4 Use the Shapiro-Wilk test
shapiro.test(MPHDIS_res)   #fail to reject means it's normal (in theory!)

#(4)
#Now we will log-transform the outcome variable DIST and name the transformed variable LOGDIST. Use the code below:
lab9b$LOGDIST = log(lab9b$DIST)


#(5). 
#Create a scatterplot of LOGDIST against MPH. Also add a regression line and a lowess line to the plot. How does this plot compare to the one based on untransformed DIST ?
lmt <- lm(lab9b$LOGDIST ~ lab9b$MPH )
plot(x = lab9b$MPH , y = lab9b$LOGDIST , main = "Moving speeds vs. Transformed stopping distances")
abline(lmt , col = "blue") 
lines(lowess(x = lab9b$MPH , lab9b$LOGDIST) , col = "red")













