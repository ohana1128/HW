HW3 <- read.csv("HW3Data.csv")
df <- data.frame(x=HW3$height,y=HW3$weight)

##1. Calculate Pearson’s correlation coefficient for weight and height.
cor(df$x,df$y,method = "pearson")

##2. Use bootstrap to build a 95% confidence interval for Pearson’s correlation coefficient.
##There are two ways to finish this, the 1st one:
library(boot)
bootFunc <- function(x,i){
  tempCor <- cor(x[i,])
  return(tempCor[1,2])
}

boot1 <- boot(df,bootFunc,stype="i",R=5000)
boot.ci(boot1)

#the 2nd way 
dfCorr <- corr(df)
boots <- matrix(0,5000,1)
for(i in 1:5000){
  dfSample <- df[sample(1:237,size = 237,replace = TRUE),]
  boots[i] <- corr(dfSample)
}

dfCorr+quantile(boots-dfCorr,c(0.025,0.975))





