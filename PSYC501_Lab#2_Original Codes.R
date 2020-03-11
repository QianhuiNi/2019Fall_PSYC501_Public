##Exercise 1
#a). Generate ONE random value from a normal distribution with mean=5 and sd=2, and save it to an object named x.
#b). Use pnorm(?, 5, 2) to find the probability p(X ≤ x)
#c). Use qnorm(?, 0, 1) to find the standard normal quantile corresponding to the probability obtained in (b).
#d). Use arithmetic operations in R to standardize the value x you generated in (a). 
#e). Is your answer in (c) same as that in (d)?
x <- rnorm(1,5,2)
x

e1_p <- pnorm(x, 5, 2)
#the probability p(X ≤ x) is 0.7854327.

e1_q <- qnorm(e1_p, 0, 1)
#the standard normal quantile corresponding to the probability obtained in (b) is 0.7906733.

e1_sd_x <- (x-5)/2
e1_sd_x

e1_q == e1_sd_x
#Yes, they are the same.

##Exercise 2
#a). Download lab2.csv and import it as a table object named lab2.
#b). Create a histogram for the reaction time (rt) data. To make your graph look more nuanced, play with the parameter br= to change the number of bins shown.
#c). Is this set of data skewed? Positively or negatively?
#d). Calculate the mean, median, and variance. Which statistic is larger: the mean or the median?
#e). Draw two vertical lines: one to indicate where the mean is located and the other to indicate where the median is located. Use this function: abline(v=?).
setwd("/Users/qianhuini/Documents")
e2_Data <- read.csv(file="lab2.csv", header=TRUE,sep = "")

hist(e2_Data$rt, br = 100)

e2_skew <- skewness(e2_Data$rt)
if (e2_skew[1] > 0){
  print("The data is skewed positively")
} else if (e2_skew[1] <0){
  print("The data is skewed negatively")
} else {
  print("The data is not skewed")
}
#The skewness of the simulated data is -0.008525844. This concludes that the rt is shewed positively.

e2_mean <- mean(e2_Data$rt)
e2_median <- median(e2_Data$rt)
e2_variance <- var(e2_Data$rt)
if (e2_mean > e2_median){
  print("its mean is larger than itsmedian")
} else if (e2_mean < e2_median){
  print("its median is larger than its mean")
} else {
  print("its mean and median are the same")
}

abline(v=e2_mean, col="red")
abline(v=e2_median,col="blue")

##Exercise 3 - Exploring mixed normal distribution:
#a). Generate 50 values from a standard normal distribution and store them in a vector X1.
#b). Generate 50 values from a normal distribution with mean=0 & sd=20. Store them in a vector X2. 
#c). Combine the two vectors to form a new vector X. [hint: use the function c(...)]
#d). Plot the data in X using the function hist(X).
#e). Calculate the mean, median, variance and standard deviation of the data in X.
#f). Plot density graphs for the three distributions you generated in this exercise in one window.

X1 <- rnorm(50,0,1)
X2 <- rnorm(50,0,20)
X <- c(X1,X2)
hist(X)
e3_mean <- mean(X)
e3_median <- median(X)
e3_sd <- sd(X)

par(mfrow=c(1,3))
hist(X1,br=10,col="coral2")
hist(X2,br=10,col="lightskyblue2")
hist(X,br=10,col = "mediumpurple2")

