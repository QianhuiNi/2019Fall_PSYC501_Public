#lab 5 script

#Exercise 1: installing packages.

install.packages("foreign")  #choose cran CA1 or CA2

### or just choose cran manually

# only have to install package once, but must load
# it everytime you restart R and want to use it!

library(foreign)   #don't actually need quotation marks here

wais = read.spss(file.choose(), to.data.frame=TRUE)

head(wais)
str(wais)
summary(wais)
dim(wais)
ncol(wais)
nrow(wais)
sum(complete.cases(wais)) #find row numer without missing value

#Exercise 2: manipulate data. 

wais.sub1 = wais[,c(1, 6, 16, 17, 40)]

#or with variable names:

wais.sub2 = wais[,c("age","sex","voc_raw","math_raw","IQ_performance")]   

#or with subset function:

wais.sub3 = subset(wais, select = c(age, sex, voc_raw, math_raw, IQ_performance))

head(wais.sub1)
head(wais.sub2)
head(wais.sub3)

#changing column names

colnames(wais.sub1) 

colnames(wais.sub1) = c("Age","Gender","Vocab","Math ","IQ")

colnames(wais.sub1)

#changing a subset of the names

colnames(wais.sub1)[3] = "Vocabulary"   #Change Vocab to Vocabulary

colnames(wais.sub1)

colnames(wais.sub1)[4:5] = c("MathScore", "IQperf")

colnames(wais.sub1)

#recoding a variable:
#  ifelse(criterion, value_crit_met, value_crit_not_met).

head(wais.sub1)

#creating a new variable for gender specifying male or female. 

wais.sub1$GenderLabel = ifelse(wais.sub1$Gender == 1, "Male", "Female")

head(wais.sub1, 7)

# NOTE: you can add a string of these together:
#e.g., 

wais.sub1$math2=ifelse(wais.sub1$MathScore <= 10, 0, ifelse(wais.sub1$MathScore <=12, 1, 2))
head(wais.sub1)

#  Extracting subsets of tables using logical operators: ==, <, <=, >, 
#  >=, &, and |. (& means ?and?, and | means ?or?)

#We want to select only female subjects? data:

wais.f = wais.sub1[wais.sub1$Gender == 2, ]

#or

wais.f = wais.sub1[wais.sub1$GenderLabel == "Female", ]

head(wais.f)
dim(wais.sub1)
dim(wais.f)

#Extract subjects with IQperf > 130

wais.130 = wais.sub1[wais.sub1$IQperf > 130, ]

head(wais.130)

#Extract subjects that are female AND have IQperf > 130 (two criteria)

wais.f130 = wais.sub1[wais.sub1$IQperf > 130 & wais.sub1$Gender == 2, ]

head(wais.f130)
dim(wais.f130)

#Extract subjects with IQperf >140 OR subjects with IQperf < 60

wais.140_60 = wais.sub1[wais.sub1$IQperf > 140 | wais.sub1$IQperf < 60, ]
head(wais.140_60)
dim(wais.140_60)

#Note: In the previous code, we used the operator "&" and here we used "|". Why 
#      is there a difference?
