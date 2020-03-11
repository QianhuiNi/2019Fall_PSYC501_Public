# lab 4 script

#Exercise 1: create a function of our own

x = rnorm(50)

Describe = function(x, plotit=TRUE){
   #Compute descriptive statistics:
       N = length(x)
       Mean = round(mean(x), 4)          #round function rounds to decimal
       Median = round(median(x), 4)      #place you specify
       Tmean = round(mean(x, tr=0.2), 4)
       SD = round(sd(x) , 4)
       Min = round(min(x) , 4)
       Max = round(max(x) , 4)
       Range = round(Max - Min, 4)
       SE = round(SD/sqrt(N), 4)          #NOTE, ERROR IN PDF DOCUMENT!!
   #Arrange the statistics in table form:
       output = data.frame( N = N, Mean = Mean,    #creates variable we want
            Median = Median, Tmean = Tmean,        # to print
            SD = SD, Min = Min, Max = Max,
            Range = Range, SE = SE,
            row.names="Descriptives: "
              )
     if(plotit){ 
       hist(x)
     }
     return(output)                        #prints our variable
}

Describe(x) #Let's try it out.

#Exercise 2

sampleDist = function( data, plotit=TRUE ) {
   set.seed(501) #set the seed so that we can replicate the result.Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.

   mat = matrix(, ncol=100, nrow=500 )
   for(i in 1:100){
      mat[,i] = sample(data, 500, replace=TRUE) 
   }

   means = apply( mat, 2, mean ) #creating sampling distributions
   medians = apply( mat, 2, median )
   tmeans = apply( mat, 2, mean, tr=.2 )

   if(plotit){
      smeans.dens = density( means ) #densities
      smedians.dens = density( medians )
      stmeans.dens = density( tmeans )

      xlimits = range( smeans.dens$x, smedians.dens$x, stmeans.dens$x )
      ylimits = range( smeans.dens$y, smedians.dens$y, stmeans.dens$y )
      plot(smeans.dens, xlim=xlimits, ylim=ylimits, main="Density plots", col="darkgreen")
      lines(smedians.dens, col="red")
      lines(stmeans.dens, col="darkblue")

      legend( "topleft",
          c("Sample means", "Sample medians", "Sample tmean"),
          col=c("darkgreen", "red", "darkblue"), lty=1, bty="n")
    }

    mean.se = round( sd(means), 4 )
    median.se = round( sd(medians), 4 )
    tmean.se = round( sd(tmeans), 4 )

    output = data.frame(SE=c(mean.se, median.se, tmean.se),
                        row.names=c("Mean", "Median", "Tmean"))

    return(output)   
}

sampleDist(runif(200000, 1, 5))

#Mixed normal

set.seed(501)              # ERROR IN PDF. MUST INCLUDE (501)
pop1=c(rnorm(10000), rnorm(800,sd=10) )   #what did we do here?

hist(pop1)

#before proceeding, take a guess: Which location estimator is the worst
#at describing central tendency?

sampleDist(pop1)

#Did the results support your guess?

#skewed distributions
# let's now examine male college students' preferred numbers 
# of sex partners in next 30 years. Import sexm.txt data. 

sexm=scan(file.choose())
hist(sexm)
sexm     #we can look at the actual values


#sample 10000 values with replacement from this dataset.

set.seed(501)
sexpop=sample(sexm,10000,replace=T)  #replace has to equal T here
 # this extrapolate from our 105 items to 10000

hist(sexpop)

sampleDist(sexpop)

#We've examined sampling distributions of different location estimators 
# under different circumstances. What do the results from last week's
# lab and today's lab suggest about choosing location estimators when
# doing data analysis?   

#mean最好的时候是perfect normal，tmean一般最好

#An example of a function with "if" and "else if" to help with your homework

x = rnorm(5)

#checks whether values are positive or not
positive = function(x){
  pos = logical(length(x))
  for(i in 1:length(x)){
    if(x[i] >= 0){
      pos[i] = T
    }
    else if(x[i] < 0){
      pos[i] = F
    }
  }
  return(pos)
}

positive(x)
