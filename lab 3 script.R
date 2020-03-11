#lab 3 script

dbinom(3,5,.6)
#x = number of successes
#size = number of trials/observations
#p = probability of success on each trial

#Exercise 1

#imageine 10 married couples, probability 4 or fewer will report being happily married (prob = .3)

dbinom(0,10,.3) + dbinom(1,10,.3) + dbinom(2,10,.3) + dbinom(3,10,.3) + dbinom(4,10,.3)

#or

sum(dbinom(0:4,10,.3))

#or can use pbinom

pbinom(4,10,.3)


#exercise 2:
#a
sum(dbinom(5:10,10,.3))

1-pbinom(4,10,.3)

#b

pbinom(8,10,.3) - pbinom(1,10,.3)


#exercise 3:
obama = runif(200000, min=1, max=5)

#create histogram
hist(obama)
#what kind of distribution is it?

mean(obama)
sd(obama)

#in reality can you ever collect such a comprehensive set of data?
# given limited time and resources, what would you do instead?

#Exercise 4. 

s1=sample(obama, 500, replace=T)
mean(s1); sd(s1)

s2=sample(obama, 500, replace=T)#replace means should sampling be with replacement
mean(s2); sd(s2)

m1=matrix(ncol=100,nrow=500)    #creates the empty matrix.

for(i in 1:100 ){         #the start of a loop, repeating i from 1 to 100
m1[,i]=sample(obama,500,replace=T)    #sample of 500
}       #close the loop

smeans=apply(m1,2,mean)       #apply function repeats the desired operation
smedians=apply(m1,2,median)    # the 2 tells R to calculate along columns
stmeans=apply(m1,2,mean,tr=.2)    #if 1 instead of 2, R calculates along rows

str(apply)        #   4 different ways to get info about a function.
apply             #  
help(apply)       #  
?apply            #

smeans
smedians
stmeans

#Creating density plots on same graph
dens.x=c(density(smeans)$x, density(stmeans)$x, density(smedians)$x)

dens.y=c(density(smeans)$y, density(stmeans)$y, density(smedians)$y)

plot(density(smeans), xlim=range(dens.x), ylim=range(dens.y), col="darkgreen", main="Density plots")

lines(density(stmeans), col="red") #add a density plot for tmeans

lines(density(smedians), col="darkblue") #add a density plot for medians

abline(v=mean(obama)) #add a vertical line for the pop. mean;

#lwd: line width.

abline(v=mean(obama),lwd=2)
abline(v=mean(obama),lwd=4)
abline(v=mean(obama),lwd=6)

legend("topright", c("Sample means", "Sample tmeans", "Sample medians"),col=c("darkgreen","red","darkblue"), lty=1)

#calculate sds of sample means, medians, and trimmed means

sd(smeans)
sd(smedians)
sd(stmeans)

#[note that I am not asking you to calculate the sample standard deviation]
# which one has the largest standard deviation? Which has the smallest?
# what is the standard deviation of a sampling distribution called?


#based on the results, when sampling from this distribution, 
#which is the best location estimator here - the mean, median, or trimmed mean?

