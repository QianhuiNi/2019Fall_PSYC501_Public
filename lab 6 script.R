## lab 6 script

#import data. 
library(foreign)

wais=read.spss(file.choose(), to.data.frame=T)
head(wais)

#extract female subjects' full IQ scores

wais.f <- wais[ (wais$sex == 2) , ]   #take all females.

IQ_f <- wais.f$IQ_full    #take only IQ_full variable.

length(IQ_f)

#Use describe() and multi.hist() from the psych package.
#what does the distribution look like.

install.packages("psych")
library(psych)

describe(IQ_f)
multi.hist(IQ_f)

	#It looks pretty normally distriubted (low skew, low kurtosis). 



#########
# Exercise 1a:

#a. write down the formula for confidence intervals when the population 
	#variance is known:

#	
#	CI = mean +/- c*(sd/sqrt(n))    #c = critical value of the 1-alpha/2 quantile. 
#	

#b. Treat IQ_f as population. Sample 500 values with replacement

IQ_f.2 <- sample(IQ_f , 5000 , replace = T)

sigma <- sd(IQ_f.2)
iq.mean <- mean(IQ_f.2)
sigma
iq.mean

#for 90% CI:
c <- qnorm(.95,0,1)

#why do we use probability .95 insead of .90?
	# two tailed test, so  we take alpha/2, 
	# therefore alpha = .1 for 90%, we take .05.

c
n <- 5000

CI.low <- iq.mean - c*(sigma/sqrt(n))
CI.up <- iq.mean + c*(sigma/sqrt(n))
CI.low
CI.up



#If-statement:    #similar logic to function ifelse() we used for recoding
a <- rnorm(1)
if(a < 0) {
	print("The value in a is smaller than 0")
} else {
	print("The value in a is not smaller than 0")
}
a       #Check the actual value of a, make sure it works

#while-loop:

#example 1:

counter <- 0 #initialize the counter
while(counter < 5) {
	print(counter)
	counter <- counter + 1
}

#Example 2:  the while loop is most useful when continuation of a procedure
		# is contingent on the result. 

check <- FALSE
while(!check) {      #while(!check) is equivalent to while(check==FALSE): its a short cut
	temp <- rnorm(20)   #takes a standard normal distribution (mean=0, sd=1)
	xbar <- mean(temp)
#  counter = counter+1
	if(xbar > 1) {
		check <- TRUE
		print("Sample found!")
		print(xbar)
	}
}          #takes samples until it gets a mean 1 sd above 0. 


CIplot <- function(data , sam_size , num_sam) {    #we will have to plug in values for sam_size and num_sam

     temp <- sample(data , sam_size * num_sam , replace = T)
     mat <- matrix(temp , ncol = num_sam)  # matrix with columns equal to num of samples
     
     smeans <- colMeans(mat)   #compute means for columns
     popmean <- mean(data)     #compute the population mean

     #Replace the shaded parts below with appropriate objects/valuess....
	# that means, you can replace .95, or x labs, etc., depending on purpose.
     cilo <- smeans - qnorm(.95) * sd(data) / sqrt(sam_size)
     cihi <- smeans + qnorm(.95) * sd(data) / sqrt(sam_size)  #these are our CI equations.

     plot(1 , 1 , xlim = range(cilo , cihi) , ylim = c(0 , num_sam) , ylab="Samples" ,  # the 1,1 specifies x and y coordinates of plot.
          xlab = "Score" , main = paste(num_sam , " CIs") ,
          type = "n") 
     i <- 1   #initalize the index (i.e., starting point
     red.count <- 0 #initialize counter of significant CIs
     while(i <= num_sam) {           #run until equal number of samples. 
        if(cihi[i] < popmean | cilo[i] > popmean ) {    #i.e., pop mean does not fall into samples CI
             lines(x = c( cilo[i] , cihi[i]) , c(i , i) , col = "red")   #creates red line for significant
             red.count <- red.count + 1
        } else {
             lines(x = c(cilo[i] , cihi[i]) , c(i , i))   #creates black line. c(i,i) gives y coordinate for each end of CI
		}
     i <- i + 1         #After each iteration, the index increases by 1
     }
     abline(v = mean(data) , lwd = 1.5 , col="blue") #add blue line at pop mean (lwd = line thickness)
     cat("Length of CI = " , cihi[1] - cilo[1] , "\n") 		#output
     cat("significant CI's = " , red.count) #number of CI's not containing true mean
}


CIplot(IQ_f, 5000, 100)

#NOTE, with 90% CI, we would expect 10% of CIs to not contain pop mean.

#Exercise 1b:

#add outliers to IQ_f
IQ_fout <- c(rep(20 , 20) , IQ_f , rep(180 , 20))  #rep replicates values e.g., value of 20, 20 times.

rep(2 , 8)

hist(IQ_fout)

describe(IQ_f)

describe(IQ_fout)

#b. before we create CIs using the new function, guess if we still
#expect approximately 10% of the CIs to not contain pop mean.

#c. use CIplot() on IQ_fout

CIplot(IQ_fout , 5000 , 100)

#compare to

CIplot(IQ_f, 5000, 100)

#outliers give us larger CIs

#Exercise 1c: effect of sample size on length of CI

CIplot(IQ_fout , 20 , 100)
CIplot(IQ_fout , 50 , 100)
CIplot(IQ_fout , 5000 , 100)
CIplot(IQ_fout , 50000 , 100)


#how do these results differ? What is the trend? what might be the logic/reason behind this trend?

# CIs get smaller with larger samples. larger samples are more representative of pop mean, especially important with outliers.

