#Exercise A.1 Data Analysis
df <- read.csv("forestfires.csv")
head(df)

#1. How many observations are there in the dataset? 517 observations
nrow(df)

#2. How many observations are there with a fire (i.e. area>0)? 270 observations
nrow(subset(df, area>0))

#3. How many observations are there with rain (i.e. rain>0)? 8 observations
nrow(subset(df, rain>0))

#4. How many observations are there with both a fire and rain? 2 observations
nrow(df[df$area>0 & df$rain>0,TRUE])

#5. Write an R code to show the columns month, day, area of all the observations.
df[,c(3,4,13)]

#6. Write an R code to show the columns month, day, area of the observations with a fire.
subset(df,area>0,select = c(month,day,area))

#7. How large are the five largest fires (i.e. having largest area)? Top five burned areas are 200.94, 212.88, 278.53, 746.28, 1090.84
tail(sort(df$area),5)

#8. What are the corresponding month, temp, RH, wind, rain, area?
subset(df,df$area %in% c("200.94","212.88","278.53","746.28","1090.84"), select = c(month,temp,RH,rain,area))

#9. Reorder factor levels of month to be from Jan to Dec.
df$month <- factor(df$month, levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
df[order(df$month), ]

#10. Add one column to the data indicating whether a fire occurred for each observation (‘TRUE’ for area>0 and ‘FALSE’ for area==0).
df$fire <- factor(df$area>0)

#11. What is the mean area, wind, temp and RH per month?
tapply(df$area, df$month, mean)
tapply(df$wind, df$month, mean)
tapply(df$temp, df$month, mean)
tapply(df$RH, df$month, mean)

#12. How many observations are there in each month?
table(df$month)

#13. How many observations are there with a fire in each month?
table(df[df$area>0, ]$month)

#Exercise A.2: Tests
#Write the analysis as if you are statistical consultant and you are writing a report for people with limited statistical knowledge. You should state what the p-value is and list the assumptions of the study. You should decide which tests to use for questions 1, 2 and 4.
#1. Test whether the average burned area changes with respect to the day of the week. Only use observations with area>0.
newdf <- subset(df, area>0)
df1 <- newdf[,c(4,13)]

#F-test
summary(aov(area~as.factor(day),data=newdf))
#P-value is 0.442 > 0.05, accept Null Hypothesis, which means that the average burned area doens't change with respect to the day of the week

#check normality by using qqplot and durbin-watson test
library(ggplot2)
library(car)
par(mfrow=c(3,3))
for(days in levels(df1$day)){tempGroup <- subset(df1,day==days,drop=TRUE); qqnorm(tempGroup$area, main=paste(days,durbinWatsonTest(tempGroup$area))); qqline(tempGroup$area);}
ggplot(df1,aes(x=factor(day),y=area))+geom_boxplot()

#check equal variance by using bartlett-test and levene-test
bartlett.test(area~day,data = df1)
library(car)
leveneTest(area~day,data=df1)

#2. Similarly, test if the average burned area is the same in each month. Again, only use observationswith area>0.
df <- read.csv("forestfires.csv")
newdf <- subset(df, area>0)
dfmonth <- newdf[,c(3,13)]
summary(aov(area~as.factor(month),data=dfmonth))
#P-value is 0.996 > 0.05, accept Null Hypothesis, which means that the average burned area doens't change with respect to the month

#check normality by using qqplot and durbin-watson test
library(ggplot2)
library(car)
par(mar=c(3,3,3,3))
par(mfrow=c(4,3))
for(months in c("apr","aug","dec","feb","jul","jun","mar","may","oct","sep")){tempGroup1 <- subset(dfmonth,month==months,drop=TRUE); qqnorm(tempGroup1$area, main=paste(months,durbinWatsonTest(tempGroup1$area))); qqline(tempGroup1$area);}
#note: the area values for "nov" and "jan" are o, so I only polt the area>0 month as requested
ggplot(dfmonth,aes(x=factor(month),y=area))+geom_boxplot()

#check equal variance by using levene-test
library(car)
leveneTest(area~month,data=newdf)

df <- read.csv("forestfires.csv")
newdf <- subset(df, area>0)

#3. Using bootstrap, obtain a 95% confidence interval for the correlation of each variable pair (FFMC,DMC, DC, ISI, temp, RH, wind, rain, area). Also compute p-values for H0 : p = 0, H1 : p = 0,where p is the Pearson correlation coefficient. Summarize the p-values in a matrix (e.g. row 2column 3 will contain the p-value for testing ⇢DMC,DC = 0).
df2 <- df[,c(5:13)]
#bootstrap t times
t <- 5000
boots <- matrix(rep(0,36*t),t,36)
k <- 1
for(j in 1:8){
  for(h in (j+1):9){
    dfjh <- df2[,c(j,h)]
    for(i in 1:t){
      df2Sample <- dfjh[sample(1:517,size = 517,replace = TRUE),]
      boots[i,k] <- cor(df2Sample)[1,2]     
    }
    k <- k+1
  }
}

#caculate confidence interval
boots.ci.output <- matrix(rep(0,72),36,2)
df2Cor <- cor(df2)
l <- 1
for(m in 1:8){
  for(n in (m+1):9){
    boots.ci.output[l,1] <- as.matrix((df2Cor[m,n]+quantile(boots[,l]-df2Cor[m,n],c(0.025,0.975))))[1,1]
    boots.ci.output[l,2] <- as.matrix((df2Cor[m,n]+quantile(boots[,l]-df2Cor[m,n],c(0.025,0.975))))[2,1]
    l <- l+1
  }
}
colnames(boots.ci.output) <- c("2.5%","97.5%")
boots.ci.output

###t-test for computing the p-avlue for hypothesis that pearson correlation coefficient is zero.
library(plyr)
colMeans <- colMeans(boots)
colSds <- apply(boots,2,sd)
pvalue <-pt(colMeans/colSds,99) 
###sotre p-value in a matrix
pvalue.output <- matrix(rep(0,81),9,9)
h <- 1
for(i in 1:8){
  for(j in (i+1):9){
    pvalue.output[i,j] <- as.matrix(pvalue <-pt(colMeans/colSds,99))[h,1]
    h <- h+1
  }
}
colnames(pvalue.output) <- c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area")
rownames(pvalue.output) <- c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area")
pvalue.output

#4. Test if the distribution of the area variable in September is the same as the distribution of area in August. Only use observations with area>0.
sep <- subset(newdf,month=="sep")
sep <- as.numeric(sep$area)
aug <- subset(newdf,month=="aug")
aug <- as.numeric(aug$area)

ks.test(sep,aug)
#p-value is 0.09754, so they have same distribution of area variable.

