#Lab 7 script

library(foreign)
data <- read.dta(file.choose())
head(data)

#run diagnostic statistics:
#load the psych package so we can use some functions. 

library(psych)
head(data)
str(data)
dim(data)
summary(data)
describe(data)

sum(!complete.cases(data))
sum(complete.cases(data))

#removing incomplete cases

chol2 <- na.omit(data)
dim(chol2)

#variables we are interested in: id, sex, wt, chol

#recoding wt variable. 
chol2$wtcat <- ifelse(chol2$wt < 89, "low" ,
                      ifelse(chol2$wt < 126, "midlow" ,
                             ifelse(chol2$wt < 158 , "midhigh" , "high"))) 
head(chol2)
unique(chol2$wtcat) 

#recoding sex variable

chol2$sex2 <- ifelse(chol2$sex == 1 , "Male" , "Female")
head(chol2) 

#recoding specific cases. 

#Option 1: long option
#create new variable named history indicating participants who have family
#history of cholesterol problems. 
chol2$history <- ifelse(chol2$id == 16 | 
                        chol2$id == 55 | 
                        chol2$id == 57 | 
                        chol2$id == 60 | 
                        chol2$id == 62 | 
                        chol2$id == 81 | 
                        chol2$id == 86 | 
                        chol2$id == 108 | 
                        chol2$id == 122 |
                        chol2$id == 153 , "YES", "NO")

#option 2, using binary operator  %in%

2 %in% c(1,3,4,6) 
2 %in% c(2,3,5,6)
c(1, 2) %in% c(2, 3, 5, 6)

#some more examples
a <- 2
mat.b <- matrix(ncol = 2 , nrow = 4)
mat.b
mat.b[ , 1] <- c(2 , 3 , 5 , 6)
mat.b

for(i in 1:nrow(mat.b)) {
       mat.b[i , 2] <- ifelse( (a %in% mat.b[i , 1]) , "YES" , "NO")
}
mat.b

a %in% mat.b[2 , 1]

historyID <- c(16, 55, 57, 60, 62, 81, 86, 108, 122, 153)
for(i in 1:nrow(mat.b)) {
       chol2$history[i] <- ifelse(chol2$id[i] %in% historyID , "YES", "NO")
}


historyID <- c(16, 55, 57, 60, 62, 81, 86, 108, 122, 153)
chol2$history <- ifelse(chol2$id %in% historyID , "YES", "NO")
chol2[ , c("id" , "history")]

#let's rearrange the data
install.packages("dplyr")
library(dplyr)
chol2 <- arrange(chol2 , id) #arrange cho12 order by id
chol2[ , c("id" , "history")]

#computing mean of cholesterol on different levels of a factor. 

#finds the mean cholesterol for each gender
tapply(chol2$chol, chol2$sex2, mean)

#finds the mean chol by levels of wtcat
tapply(chol2$chol, chol2$wtcat, mean)

#finds the mean chol by family history of cholesterol issues
tapply(chol2$chol, chol2$history, mean)

#finds the mean chol by combos of sex & weight category
tapply(chol2$chol, list(chol2$sex2, chol2$wtcat), mean)

################################
# Confidence intervals when the population sd is unknown; with normality

#Exercise 1:
#1a. sample 10,000 values from normal distribution mean of 10 and sd 5.

pop1 <- rnorm(10000 , 10 , 5)

#1b. write the forumla for t statistic

#(xbar-mu) / (sd/sqrt(n))  with df = n-1

#we use the t statistic when we do not know the population sd

#1c. draw sample of 20 values, calculate t statistic.
sam1 <- sample(pop1 , 20)
tstat <- (mean(sam1) - mean(pop1)) / (sd(sam1) / sqrt(length(sam1)))
tstat
mean(sam1)
mean(pop1)
sd(pop1)
sd(sam1)

#1d: create function for taking many samples and t statistics.

t_stat <- function(x , sam_size, num_sam) {
     temp <- sample(x , sam_size * num_sam , replace = TRUE)
     mat <- matrix(temp , ncol=num_sam)
     mu <- mean(x)           #Population mean  
     xbar <- colMeans(mat) #sample means (compute the mean of each column)
     s <- apply(mat , 2 , sd)
     t <- (xbar - mu) / (s / sqrt(sam_size))   #Compute t-scores for all samples
     return(t)
}

#1e. take 5000 samples of 20

tscores1 <- t_stat(pop1 , 20 , 5000)

#1f. plot the t distribution.
tplot <- function(t_statistics , conf.level = .95 , df) {
  cols <- hcl(h = c(0, 240), l = 60, alpha = 0.3)
  conf.hi <- 1 - (1 - conf.level) / 2
  conf.lo <- 1 - conf.hi
  dens.x <- density(t_statistics , n = 10000)$x
  dens.y <- density(t_statistics , n = 10000)$y
  n1 <- length(dens.x)
  c.t.lo <- qt(conf.lo , df = df)
  c.t.hi <- qt(conf.hi , df = df)
  
  plot(dens.x , 
       dens.y , 
       xlim = range(dens.x) , 
       ylim = range(dens.y , dt(0 , df = df)) , 
       type = "l" , 
       lwd = 0.5 ,
       main = paste("Distribution of t-statistics with df=" , df , sep = "") , 
       xaxt = "n" ,
       xlab = "")
  
  axis(1 , 
       mgp = c(3 , 2.5 , 0) , #label location, tick mark labels, tick mark location
       at = c(qt(conf.lo , df = df) , 0 , qt(conf.hi , df = df)) , 
       c(paste("critical \n quantile" , "\n" , round(qt(conf.lo , df = df) , 3)) , 
         0 , 
         paste("critical \n quantile" , "\n" , round(qt(conf.hi , df = df) , 3))) ,
       col = "red"  
  )
  
  curve(dt(x , df = df) , 
        from = qt(1e-05 , df = df) ,
        to = qt(1-1e-05 , df = df) ,
        add = TRUE , 
        lwd = 1.5)
  
  dens.xt <- seq(c.t.lo , c.t.hi , (c.t.hi - c.t.lo) / 10000)
  dens.yt <- dt(dens.xt , df = df)
  n2 <- length(dens.xt)
  t_statistics <- sort(t_statistics)
  lo <- max(dens.x[dens.x < t_statistics[round(length(t_statistics) * conf.lo)]])
  hi <- min(dens.x[dens.x > t_statistics[round(length(t_statistics) * conf.hi)]])
  
  polygon(x = c(dens.xt[1] , dens.xt , dens.xt[n2]) , 
          y = c(0 , dens.yt , 0) , 
          col = cols[1] , 
          lwd = 0.5)
  
  polygon(x = c(lo , dens.x[which(dens.x == lo) : which(dens.x == hi)] , hi) ,
          y = c(0 , dens.y[which(dens.x == lo) : which(dens.x == hi)] , 0) , 
          col = cols[2] , 
          lwd = 0.5)
  
  legend("topleft" , 
         c(paste("Theoretic t-dist with df =" , df) , 
           paste("t-dist based on data with df =" , df)) , 
         pch = 22 , 
         pt.cex = 2.5 , 
         col = "black" , 
         pt.bg = cols , 
         bty = "n")
  
  actual_prob <- length(which(t_statistics >= qt(conf.lo , df = df) &
                                t_statistics <= qt(conf.hi,df = df))) / length(t_statistics)
  print(paste("The actual probability coverage:" , actual_prob))
}


tplot(tscores1 , conf.level = 0.9 , df = 19) 


#1g. What conclusions can you make about computing confidence intervals given
#the kind of population distribution given in this exercise? In other words,
#is the student's t satisfactory given the particular population data?
