#Lab 8 script

lab8a <- read.table(file.choose(),header=T)
lab8b <- read.table(file.choose(), header=T)

head(lab8a)
head(lab8b)

source(file.choose()) # read in package

# Exercise 1: lists

#Lists can store other types of data structures, including vectors, data frames, etc.,

#create a list

V1 <- rnorm(20)     #a vector
T1 <- data.frame(X = 1:10 , Y = letters[1:10])    #a data frame
M1 <- matrix(runif(20) , ncol = 4)   #a matrix

L1 <- list(V1 = V1 , T1 = T1 , M1 = M1)
L1

rm(V1, T1, M1)   #can remove the original objects since they are now saved in a list

#lists are a great way to organize your data, e.g., you can 
#store different data sets or test results from the same 
#study in one list, to minimize clutter

#How do we extract an item of a list? Use "[[]]" or "$"

L1[[1]] #the first element in list
L1[["T1"]]
L1$M1

#How do you extract a subset of an item in a list?

#extract the 4th value of the vector V1 in L1

L1[[1]][4]
L1$V1[4]
L1[["V1"]][4]

#How do you extract (1) the column X in T1? (2) the first 5 rows of T1?

L1$T1$X

head(L1$T1, 5)

#Check the size of the list

length(L1)    #Note, length is the number of objects in list

#Check/change item names

names(L1)

names(L1) <- c("Vector" , "Table" , "Matrix")

names(L1)

#apply a function to a list using the function lapply(x,fun,...)

lapply(L1 , length)

#looking at the data set we imported.

head(lab8a)
summary(lab8a)
str(lab8a)
dim(lab8a)

#1 use describe() to compute descriptive stats for each subset. 

#Way 1, use logical operators:

lab8a.1a <- lab8a[lab8a$set == "a" , ]
dim(lab8a.1a)

lab8a.1b <- lab8a[lab8a$set == "b" , ]
dim(lab8a.1b)

lab8a.1c <- lab8a[lab8a$set == "c" , ]
dim(lab8a.1c)

library(psych)
describe(lab8a.1a)
describe(lab8a.1b)
describe(lab8a.1c)

#way 2, using function subset()

lab8a.2a <- subset(lab8a , lab8a$set == "a")
dim(lab8a.2a)

lab8a.2b <- subset(lab8a , lab8a$set == "b")
dim(lab8a.2b)

lab8a.2c <- subset(lab8a , lab8a$set == "c")
dim(lab8a.2c)

describe(lab8a.2a)
describe(lab8a.2b)
describe(lab8a.2c)

#way 3, using the tapply() function

tapply(lab8a$data , lab8a$set , describe)

#2. Create a histogram for each subset. make sure that all 3 
# histograms are side by side in one window

par(mfrow = c(1 , 3))  #create the graphing window for 3 graphs on one row.

tapply(lab8a$data , lab8a$set , hist)

#note: with tapply() function we can use describe function and graph with just two lines of code

#Learning the split function.   split(x,f)  x is data, f is factor splitting on

temp <- split(lab8a$data , f = lab8a$set)  

names(temp)
length(temp$a)

#Exercise 2: one sample t-test (classical method). 

#we learned to compute t-statistics last week. We can also run t tests easily

#use t.test function

#t.test(x, alternative=c("two.sides", "less", "greater"), mu=0, conf.level=.95)
	#"alternative" specifies two-tailed or one-tailed test. defaults to two tailed
	# mu  defaults to 0, conf.level defaults to .95

mean(L1$Vector)

t.test(L1$Vector , mu = 0)   #what do the results tell us?

t.test(L1$Vector , mu = 2)   #what do the results tell us?

#How to make statistical decision: 
#   Statistical decision:
#   i.	Whether the confidence interval contains the hypothesized population mean;
#   ii.	Whether the p-value is smaller than the alpha level you set;
#   iii.	Whether the absolute value of tobserved is greater than the critical 

qt(.975 , df = 19)  #to get the critical quantile

# How do we access individual components of a t-test output?
# the results that R gives us are in list form.

result.y <- t.test(L1$Vector , mu = 2)
names(result.y)   #Gives us the names of different components
			#i.e., if only wanted the pvalue, then use "p.value"

result.y$statistic
result.y$p.value
result.y$conf.int

# Now that we know how to access different parts of t-test results, 
# we will check how the one-sample t-test performs in various situations. 
# The data in Set a, Set b, and Set c all have the mean of 0, but they 
# differ in their distributions (a: normal; b: contaminated normal; c: skewed). 
# Let?s treat them as three population distributions. Let?s take a look at 
# the Type 1 error rates when testing H0: mu = 0. 

#Remember, since the three distributions all have the population mean of 0, 
# that means we are testing H0 when H0 is in fact true. Hence, any 
#rejections are considered Type 1 error.

pval.a <- pval.b <- pval.c <- numeric(10000) #initialize p-value storing vectors, set all equal to 10000 
i <- 1    #Initialize the counter for the while loop. 
while(i <= 10000){    
     temp.a <- sample(temp$a , 20 , replace = TRUE) #20 values from normal dist.
     temp.b <- sample(temp$b , 20 , replace = TRUE) #20 values from cnormal dist.
     temp.c <- sample(temp$c , 20 , replace = TRUE) #20 values from skewed dist.
     pval.a[i] <- t.test(temp.a , mu = 0)$p.value  #one-sample t.test
     pval.b[i] <- t.test(temp.b , mu = 0)$p.value  #saves only the p value
     pval.c[i] <- t.test(temp.c , mu = 0)$p.value
     i <- i + 1    #Increase the counter by one each iteration
}
sum(pval.a < 0.05) / 10000  #Actual type 1 rate normal dist
sum(pval.b < 0.05) / 10000	# contaminated normal, reject too few (low power)
sum(pval.c < 0.05) / 10000	# skewed, reject too many (inflated alpha)

#Exercise 3: Robust methods

head(lab8b)

#descriptives for D1
describe(lab8b$D1)

par(mfrow = c(1 , 1))
hist(lab8b$D1)

outbox(D1)    #Checks for outliers

#do a classic t test
t.test(lab8b$D1 , mu = .9)

#we reject the null hypothesis

#use a robust mthod

#from wilcox's source code

#trimci(x, tr = 0.2, alpha = 0.05, null.value = 0, pr = T), 
#x 		data
#tr 		amount of trimming; 20% by default;
#alpha 	nominal type 1 error rate; 0.05 by default;
#null.value hypothesized population mean; 0 by default;
#pr		whether a header is printed; TRUE by default

trimci(lab8b$D1 , null.value = .9)  #fail to reject. 

#Now let's test D2
describe(lab8b$D2)
hist(lab8b$D2)
outbox(lab8b$D2) 
t.test(lab8b$D2 , mu = -.7)
trimci(lab8b$D2 , null.value = -.7)

#based on the results form testing D1 and D2, how can outliers affect hypothesis testing?
