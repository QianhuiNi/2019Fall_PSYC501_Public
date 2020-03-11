library(quantreg)
library(psych)
library(WRS)
lab10hw <- read.csv(file="lab10hw.csv", header=TRUE)
head(lab10hw)
lab10hw.lm <- lm(score ~ iq , data = lab10hw)
summary(lab10hw.lm)

plot(x = lab10hw$iq , y = lab10hw$score , main = "Scatter plot of Scores vs.IQ", 
     xlab="IQ", ylab="Scores in the test")
abline(lab10hw.lm$coef , col = "red")
lab10hw.lowess <- lowess(x = lab10hw$iq , y = lab10hw$score)
lines(lab10hw.lowess , col = "blue")

#Linearity
lab10hw.res <- residuals(lab10hw.lm)     
lab10hw.pred <- predict(lab10hw.lm) 
plot(x = lab10hw.pred , y = lab10hw.res , 
     main = "Predicted values vs. Residuals" , pch = 3)
abline(h = 0 , col = "red")
lines(lowess(x = lab10hw.pred , y = lab10hw.res) , col = "blue" )
#Normality
describe(lab10hw.res)
multi.hist(lab10hw.res)
#use lm.daignose() function
lm.diagnose = function(object, plot=TRUE){
  installed = rownames(installed.packages())	
  if("psych" %in% installed) require("psych") 
  else {
    cat("[Installing 'psych' for descriptive statistics]\n")
    install.packages("psych", repos = "http://cran.cnr.Berkeley.edu")
    require("psych")
  }
  if("nortest" %in% installed) require("nortest") 
  else {
    cat("[Installing 'nortest' for tests for normality]\n")
    install.packages("nortest", repos = "http://cran.cnr.Berkeley.edu")
    require("nortest")
  }
  if(class(object)=="formula") model=lm(object) 
  else if(class(object)=="lm") model=object
  else stop("A fitted lm model is required!") 
  
  model.res=residuals(model)
  model.pred=predict(model)
  
  if(plot){
    #Scatter plot of residuals against predicted values along with a lowess
    par(mfrow=c(2,2), mar=c(4,4.5,3,1))
    dens.y = hist(model.res, plot=F)$density
    plot(x=model.pred, 
         y=model.res, 
         xlab="Predicted Values", 
         ylab="Residuals",
         main="Scatterplot of Residuals vs. Predicted",
         cex.main=1,
         pch=3
    )
    abline(h=0, col="red")
    lines(lowess(x=model.pred, y=model.res))
    
    #Q-Q Plot
    qqnorm(model.res, ylab="Residuals", cex.main=1)
    qqline(model.res)
    
    #Histogram and normal curve
    hist(model.res, 
         freq=FALSE, 
         ylim=range(dens.y, dnorm(mean(model.res), mean=mean(model.res), sd=sd(model.res))),
         main="Histogram of Residuals and Normal Fit", 
         cex.main=1,
         xlab="Residuals",
         ylab="Density"
    )		 
    curve(dnorm(x, mean=mean(model.res), sd=sd(model.res)), add=TRUE)
    
    #Boxplot
    boxplot(model.res, main="Boxplot of Residuals", cex.main=1)
  }		
  cat("\n")
  print(model$call)
  
  #Descriptives and normality tests
  describe.stats=apply(t(describe(model.res)[,c(3,5,4,13,8:9,11,12)]), 2, round, 3)
  colnames(describe.stats) = " "
  #rownames(describe.stats)=" "
  
  normtests = list(shapiro.test(model.res), 
                   lillie.test(model.res), 
                   ad.test(model.res),
                   cvm.test(model.res))
  norm.stats = unlist(lapply(normtests, "[[", "statistic"))
  norm.ps = unlist(lapply(normtests, "[[", "p.value"))
  
  cat("\nDescriptive Statistics of Residuals\n")
  describe.output=data.frame(describe.stats, 
                             row.names=paste("       ", rownames(describe.stats)))
  colnames(describe.output) = " "
  print(describe.output)
  norm.rownames = c("Shapiro-Wilk","Kolmogorov-Smirnov","Anderson-Darling","Cramer-von Mises")
  norm.results=data.frame(statistic=norm.stats, 
                          p=norm.ps,
                          row.names=norm.rownames
  )
  norm.output = data.frame(statistic=round(norm.stats, 4),
                           p=format.pval(round(norm.ps, 4), 4),
                           ifelse(norm.ps < 0.0001, "***",
                                  ifelse(norm.ps < 0.001, "**",
                                         ifelse(norm.ps < 0.01, "**",
                                                ifelse(norm.ps < 0.05, "*",
                                                       ifelse(norm.ps < 0.1, ".", " "))))),
                           row.names=norm.rownames)
  colnames(norm.output)[3] = c(" ")
  output=list(Descriptives = describe.stats, NormalityTests = norm.results)
  cat("\n\t\t\tTests for Normality\n")
  print(norm.output)
  cat("---\n.0001 '***' .001 '**' .01 '*' .05 '.' .1\n\n")
  invisible(output)
}

lm.diagnose(lab10hw.lm)

leveragepoints <- out(lab10hw$iq)
leveragepoints_id <- c(12, 17, 28, 29, 41)
lab10hw2 <- lab10hw[-leveragepoints_id, ]

lab10hw2.lm <- lm(score ~ iq , data = lab10hw2)
summary(lab10hw2.lm)

plot(x = lab10hw2$iq , y = lab10hw2$score , 
     main = "Scatter plot of Scores vs.IQ using new dataset", 
     xlab="IQ", ylab="Scores in the test")
abline(lab10hw2.lm$coef , col = "red")
lab10hw2.lowess <- lowess(x = lab10hw2$iq , y = lab10hw2$score)
lines(lab10hw2.lowess , col = "blue")

lm.diagnose(lab10hw2.lm)

lsfit(lab10hw2$iq,lab10hw2$score)$coef