CSC2005 <- read.csv("CSC2005.csv", sep=",")
dim(CSC2005)
colnames(CSC2005)

CSC2005[CSC2005=="PrivacySuppressed"] <- NA #replace some unuseless strings with "NA"
CSC2005[CSC2005=="NULL"] <- NA #replace NULL strings with "NA"

sapply(CSC2005,class) #check data type
f=function(x){#create a function to convert data
  x <- as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x #display the column
}
CSC2005[,10:66] <- data.frame(apply(CSC2005[,10:66],2,f)) #apply the function to the dataset

sapply(CSC2005, function(x) sum(is.na(x))) #check whether the data has missing values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(CSC2005,2,pMiss)

colnames(CSC2005)
CSC2005 <- CSC2005[,-c(10,11,39,40)]
colnames(CSC2005)

E6 <- CSC2005[,1:61] #removed variable "md_earn_wne_p8"
colnames(E6)
E6.temp <- subset(E6,!is.na(E6$md_earn_wne_p6)) #removed rows without value for "md_earn_wne_p6"
library(VIM) #check the missing value patterns by ploting graphs
aggr_plot <- aggr(E6, col=c('yellow','purple'), numbers=TRUE, sortVars=TRUE, labels=names(E6.temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
library(mice) #impute missing values for other numerica variables
tempData <- mice(E6.temp[,10:61],m=5,meth='pmm',seed=500)
E6.final <- E6.temp 
E6.final[,10:61] <- complete(tempData,1) #put the imputed values back to the the dataset
apply(E6.final,2,pMiss) #double check whether there are still some missing values
E6.final <- E6.final[,-40]
write.csv(E6.final,file = "6yearearning2005.csv")

E8 <- CSC2005[,-61] #removed variable "md_earn_wne_p6"
colnames(E8)
E8.temp <- subset(E8,!is.na(E8$md_earn_wne_p8)) #removed rows without value for "md_earn_wne_p6"
library(VIM) #check the missing value patterns by ploting graphs
aggr_plot <- aggr(E8, col=c('yellow','purple'), numbers=TRUE, sortVars=TRUE, labels=names(E6.temp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
library(mice) #impute missing values for other numerica variables
tempData <- mice(E8.temp[,10:61],m=5,meth='pmm',seed=500)
E8.final <- E8.temp 
E8.final[,10:61] <- complete(tempData,1) #put the imputed values back to the the dataset
apply(E8.final,2,pMiss) #double check whether there are still some missing values
E8.final <- E8.final[,-40]
write.csv(E8.final,file = "8yearearning2005.csv")

