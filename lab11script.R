#lab 11 script
#bootstrapping. 


#load wilcox source code:

source(file.choose())
library(psych)

#Bootstrapping utilizes resampling.

#write a function to resample our data (this should be very familiar). 

resample <- function(data , sam_size , num_sam) {
  mat <- matrix(nrow = sam_size , ncol = num_sam)
  for(i in 1:num_sam) {
    mat[ , i] <- sample(data , sam_size , replace = TRUE)
  }
  mat <- matrix(mat , nrow = sam_size , ncol = num_sam)
}

#Let?s try the function:
v1 <- 1:20
v1
m1 <- resample(v1 , sam_size = 20 , num_sam = 3)
m1

#percentile bootstrap with trimmed mean. 

#a. check lab11a. create histogram, compute descriptive stats (use describe).

lab11a <- scan(file.choose())

lab11a
describe(lab11a)
hist(lab11a)

#b. now take 2000 samples from lab11a and save them in matrix M1. 
#	what should sample size be? .... it should be the size of the data set.

M1 <- resample(lab11a , 20 , 2000)

#c compute 20% trimmed means for each sample, save as tmeans.

tmeans <- apply(M1 , 2 , tmean)
length(tmeans)

#d. put bootstrap trimmed means in ascending order.

tmeans.sorted <- sort(tmeans)

#e. find the 95% CI for the pop trimmed mean. In other words, find Xbar*t(l+1) and Xbar*t(u). 

#L=alpha*B/2   #round to nearest integer. calculate lower bound, B=bootstrap samples
#U=B-L       #calculate upper bound. 

#L
.05*2000/2

#U
2000-50

#f. what is value of L and U:

tmeans.sorted[51]   #x at L+1
tmeans.sorted[1950]  #x at U

#g. if null hypothesis is = 1.8 at alpha .05, do we reject?

#h. do what we just did with a previously written function:

trimpb(lab11a , null.value = 1.8)


#Exercise 2.

#there are two types of bootstrap-t methods: equal-tailed and symmetric. 


#2.1 equal-tailed bootstrap. 

#use lab11a and samples stored in M1. 

#Formula on worksheet:  #notice t(L) is always negative, which is why we subtract it from the mean to get the upper bound. 

#let's do an example where null = 1.8 at alpha of .05

#compute trimmed mean of original dataset

xbart <- tmean(lab11a)
xbart

tse <- trimse(lab11a)

#calculate t statistic for each sample  (using trimci() this time).

results.11a <- apply(M1 , 2 , trimci , null.value = xbart , pr = F)
length(results.11a)
class(results.11a)
#saved as a list, remember how to recall objects from list?

results.11a[[1]]

#remember, we want the t value (i.e., test.stat)

results.11a[[1]][["test.stat"]]

#extract all the test.stat

stats.11a <- lapply(results.11a , "[[" , "test.stat")    #second argument is function, here we specify we want variable name, and 3rd argument we specify which variable.
length(stats.11a)
head(stats.11a)
class(stats.11a)

stats.11a <- unlist(stats.11a)
head(stats.11a)
class(stats.11a)

#put t statistics in ascending order. 

t_stats <- sort(stats.11a)

#compute 95% CI. need to compute L and U first.

L <- .05 * 2000 / 2
U <- 2000 - L
L ; U

t_stats[L + 1]
t_stats[U]

xbart - (t_stats[U] * tse)
xbart - (t_stats[L + 1] * tse)

#do we reject null hypotehsis of 1.8?

#Can also use:

trimcibt(lab11a , nullval = 1.8 , side = F) #equal-tailed bootstrap-t; no p-value
trimcibt(lab11a , nullval = 1.8 , side = T) #symmetric bootstrap-t
