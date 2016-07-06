######P1.Data Cleaning######
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

######P2.EDA######
e6 <- read.csv("6yearearning2005.csv")
e6 <- e6[,-1]
dim(e6)
str(e6)
summary(e6)

df6 <- e6[,5:60]
UNTID <- as.factor(e6[,1])
row.names(df6) <- UNTID

###Descriptive Statistics###
library(ggplot2)
library(reshape)
df6.melt <- melt(df6)
ggplot(df6.melt,aes(x=df6.melt$variable,y=value))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1,color = "black"),axis.text.y = element_text(color = "black")) + labs(x="variables")

sdf6 <- as.data.frame(scale(df6)) #scale data to make sure they have the same means
sdf6.melt <- melt(sdf6)
ggplot(sdf6.melt,aes(x=sdf6.melt$variable,y=value))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1,color = "black"),axis.text.y = element_text(color = "black")) + labs(x="scaled variables")
ggplot(sdf6.melt,aes(x=sdf6.melt$variable,y=value))+geom_boxplot()+facet_wrap(~variable,scales="free") + labs(x="scaled variables")

ggplot(sdf6.melt,aes(x=value))+geom_density()+facet_wrap(~variable,scales="free") + labs(x="scaled variables")

library(corrplot) ##visualizations for correlation matrices
corrplot(cor(sdf6),cl.cex = 0.8,tl.cex = 0.5)

###PCA###
library(stats)
fit <- princomp(~.,sdf6, cor =TRUE)
summary(fit)
par(mar=c(3,3,3,3))
plot(fit,type="lines")
loadings(fit)
fit$scores[1:10,]
par(mfrow=c(1,2))
biplot(fit)
biplot(fit,expand = 4, xlim=c(0.02,0.04),ylim=c(-0.02,0.02))

###Clustering###
#"Complete Shrinkage" and "Single Shrinkage"
sdf6.cluster <- sdf6[1:10,] #data has been scaled above
hc.complete <- hclust (dist(sdf6.cluster),method ="complete")
hc.single <- hclust (dist(sdf6.cluster), method ="single")
par(mfrow =c(1,2))
par(mar=c(2,3,3,3))
plot(hc.complete ,main ="Complete Linkage", xlab="", sub ="",
     cex =.9)
rect.hclust(hc.complete, k=5, border = "red")
plot(hc.single , main=" Single Linkage ", xlab="", sub ="",
     cex =.9)
rect.hclust(hc.single, k=5, border = "red")
#K-means cluster
sdf6.temp <- sdf6
wss <- (nrow(sdf6.temp)-1)*sum(apply(sdf6.temp,2,var,na.rm=TRUE))
sdf6.kk <- na.omit(sdf6.temp) #kmeans doesn't work for NA values in the R
for(i in 2:10) wss[i] <- sum(kmeans(sdf6.kk,centers=i)$withinss)
par(mfrow =c(1,1))
plot(1:10,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
sdf6.kktemp <- scale(sdf6.kk);fit <- kmeans(sdf6.kktemp,3)
library(cluster); 
par(mar=c(5,5,4,4))
clusplot(sdf6.kktemp, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0,col.p = fit$cluster,cex = 0.5)
fit$size

######P3.Regression Model######
sdf6.melt2 <- cbind(sdf6.melt,md_earn_wne_p6=sdf6$md_earn_wne_p6)
ggplot(sdf6.melt2,aes(x=value,y=md_earn_wne_p6))+geom_point()+facet_wrap(~variable,scales="free")
ggplot(sdf6.melt2,aes(x=value,y=md_earn_wne_p6))+stat_bin2d()+facet_wrap(~variable,scales="free")

###Variable Selection###
library(MASS) #forward stepwise selection by looking for lowest AIC
templm <- lm(md_earn_wne_p6~1,data=sdf6)
tempScope <- formula(lm(md_earn_wne_p6~.,sdf6))
step <- stepAIC(templm,scope = tempScope,direction="forward")
step$anova #AIC=-7428.51
##forward selection model: 
md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
  PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
  dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
  UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
  UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
  UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
  main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
  PCIP48 + pct_hispanic
library(glmnet)
glm.fwd <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                 PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                 dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                 UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                 UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                 UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                 main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                 PCIP48 + pct_hispanic,data=sdf6) #compared with back select, -PCIP48, - PCIP52,
library(boot)
cv.glm(sdf6,glm.fwd,K=5)$delta[1] #MSE=0.2588807

templm <- lm(md_earn_wne_p6~.,data=sdf6) #backward stepwise selection
tempScope <- formula(lm(md_earn_wne_p6~1.,sdf6))
step <- stepAIC(templm,scope = tempScope,direction="back")
step$anova        
##backward selection model:
md_earn_wne_p6 ~ main + NUMBRANCH + PREDDEG + HIGHDEG + CONTROL + 
  PCIP11 + PCIP14 + PCIP15 + PCIP22 + PCIP26 + PCIP27 + PCIP40 + 
  PCIP47 + PCIP48 + PCIP49 + PCIP50 + PCIP51 + PCIP52 + UGDS + 
  UGDS_NRA + UGDS_UNKN + UGDS_WHITENH + UGDS_BLACKNH + UGDS_API + 
  UGDS_HISPOld + TUITIONFEE_IN + TUITIONFEE_OUT + C150 + UG25abv + 
  CDR2 + loan_ever + pell_ever + age_entry + agege24 + female + 
  married + dependent + veteran + first_gen + md_faminc + pct_white + 
  pct_ba + pct_born_us + median_hh_inc + unemp_rate
glm.back <- glm(md_earn_wne_p6 ~ main + NUMBRANCH + PREDDEG + HIGHDEG + CONTROL + 
                  PCIP11 + PCIP14 + PCIP15 + PCIP22 + PCIP26 + PCIP27 + PCIP40 + 
                  PCIP47 + PCIP48 + PCIP49 + PCIP50 + PCIP51 + PCIP52 + UGDS + 
                  UGDS_NRA + UGDS_UNKN + UGDS_WHITENH + UGDS_BLACKNH + UGDS_API + 
                  UGDS_HISPOld + TUITIONFEE_IN + TUITIONFEE_OUT + C150 + UG25abv + 
                  CDR2 + loan_ever + pell_ever + age_entry + agege24 + female + 
                  married + dependent + veteran + first_gen + md_faminc + pct_white + 
                  pct_ba + pct_born_us + median_hh_inc + unemp_rate,data=sdf6)
cv.glm(sdf6,glm.back,K=5)$delta[1] #MSE=0.2586969

library(glmnet) #LASSO
lasso.cv <- cv.glmnet(x=as.matrix(sdf6[,-56]),y=as.matrix(sdf6[,56]),alpha=1,nfolds = 5)
par(mar=c(4,4,2,2))
plot(lasso.cv)
min(lasso.cv$cvm) #MSE=0.25826

###Model Modification
lm <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
           PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
           dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
           UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
           UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
           UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
           main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
           PCIP48 + pct_hispanic,data=sdf6)
summary(lm)

sdf6.melt3 <- cbind(sdf6.melt,resid=lm$residuals) #check the patterns
ggplot(sdf6.melt3,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")

#polynomial regression
PCIP26MSE <- rep(0,10) #transforming PICP26
for(i in 1:10){
  templm <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                  PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                  dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                  UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                  UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                  UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                  main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                  PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,i),data=sdf6)
  tempCV <- cv.glm(sdf6,templm, K=5)
  PCIP26MSE[i] <- tempCV$delta[1]
}
plot(PCIP26MSE)
which.min(PCIP26MSE) #2nd
min(PCIP26MSE) #0.2546

UGDSMSE <- rep(0,10) #transforming UGDS_MRA
for(i in 1:10){
  templm <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                  PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                  dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                  UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                  UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                  UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                  main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                  PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) - UGDS_NRA + poly(UGDS_NRA,i),data=sdf6)
  tempCV <- cv.glm(sdf6,templm, K=5)
  UGDSMSE[i] <- tempCV$delta[1]
}
plot(UGDSMSE)
which.min(UGDSMSE) #1st
min(UGDSMSE)

polylm <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
               PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
               dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
               UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
               UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
               UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
               main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
               PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2), data=sdf6)
polyglm <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                 PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                 dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                 UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                 UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                 UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                 main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                 PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2), data=sdf6)
cv.glm(sdf6,glm.back,K=5)$delta[1] #MSE=0.25713

#spline
library(gamclass)
library(mgcv)
library(MASS)
gam.lm <- gam(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                main + C150 + s(PCIP26) + loan_ever + PCIP40 + first_gen + poverty_rate + 
                PCIP48 + pct_hispanic,data=sdf6)
summary(gam.lm)
CVgam(formula(gam.lm),sdf6,nfold=5) #0.2605

#add interaction terms
glm.inter1 <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main,data=sdf6)
cv.glm(sdf6,glm.inter1,K=5)$delta[1] # MSE=0.2551777, NUMBRANCH:main

glm.inter2 <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + loan_ever:pell_ever,data=sdf6)
cv.glm(sdf6,glm.inter2,K=5)$delta[1] # MSE=0.2556128, NUMBRANCH:main + loan_ever:pell_ever

glm.inter3 <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + UG25abv:dependent,data=sdf6)
cv.glm(sdf6,glm.inter3,K=5)$delta[1] #MSE=0.2576299, NUMBRANCH:main + UG25abv:dependent

glm.inter4 <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + age_entry:agege24 ,data=sdf6)
cv.glm(sdf6,glm.inter4,K=5)$delta[1] #MSE=0.2572401, NUMBRANCH:main + age_entry:agege24

glm.inter5 <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6)
cv.glm(sdf6,glm.inter5,K=5)$delta[1] #MSE=0.2501273, NUMBRANCH:main + first_gen:md_faminc

interlm <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6)
summary(interlm) #Adjusted R-squared:  0.756

library(car)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
qqPlot(interlm) #heavy tails, errors are too way off.
*cutoff <- 4/((nrow(sdf6)-length(interlm$coefficients)-1))
*plot(interlm,which=4,cook.level=cutoff) #cook's distance
plot(interlm,which=4)
sortedRes <- sort(interlm$residuals)
head(sortedRes)
tail(sortedRes)
sdf6[c("105516","188526","221670"),]
df6[c("105516","188526","221670"),]
remove1 <- c("105516","188526","221670")
sdf6WO1 <- sdf6[!rownames(sdf6) %in% remove1,]
interlmWO1 <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                   PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                   dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                   UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                   UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                   UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                   main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                   PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6WO1)
summary(interlmWO1)
qqPlot(interlmWO1) #heavy tails, keep removing

plot(interlmWO1,which=4)
remove2 <- c("179265","233338","28828201")
sdf6WO2 <- sdf6WO1[!rownames(sdf6WO1) %in% remove2,]
interlmWO2 <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                   PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                   dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                   UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                   UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                   UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                   main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                   PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6WO2)
summary(interlmWO2)
qqPlot(interlmWO2) #heavy tails, keep removing

plot(interlmWO2,which=4)
remove3 <- c("112525","423643","22828201")
sdf6WO3 <- sdf6WO2[!rownames(sdf6WO2) %in% remove3,]
interlmWO3 <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                   PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                   dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                   UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                   UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                   UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                   main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                   PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6WO3)
summary(interlmWO3)
qqPlot(interlmWO3) #heavy tails, keep removing

#transform the response
df6.log <- df6
df6.log$md_earn_wne_p6 <- log(df6$md_earn_wne_p6)
sdf6.log <-as.data.frame(scale(df6.log))

interlm.log <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                    PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                    dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                    UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                    UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                    UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                    main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                    PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6.log)
summary(interlm.log)
interglm.log <- glm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                      PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                      dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                      UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                      UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                      UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                      main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                      PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6.log)
cv.glm(sdf6,interglm.log,K=5)$delta[1] #MSE=0.2498101
qqPlot(interlm.log) #much better than interlmWO3, but still has some heavy tail.

plot(interlm.log,which=4)
remove1.log <- c("105516","221670","233338")
sdf6WO1.log <- sdf6.log[!rownames(sdf6.log) %in% remove1.log,]
interlmWO1.log <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                       PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                       dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                       UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                       UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                       UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                       main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                       PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6WO1.log)
qqPlot(interlmWO1.log)
plot(interlmWO1.log,which=4)
remove2.log <- c("123633","194675","22828201")
sdf6WO2.log <- sdf6WO1.log[!rownames(sdf6WO1.log) %in% remove2.log,]
interlmWO2.log <- lm(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                       PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                       dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                       UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                       UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                       UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                       main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                       PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc ,data=sdf6WO2.log)
summary(interlmWO2.log)
qqPlot(interlmWO2.log) #still has heavy tail.
shapiro.test(interlmWO2.log$residuals) #sample size is larger than 5000, us K-S test
normality <- rnorm(30,mean=0)
ks.test(normality,interlmWO2.log$residuals) # p-value = 0.007681 < 0.05, reject null F1=F2, not normal
ncvTest(interlmWO2.log) # no eqaul variance 

#quantile regression
library(quantreg)
attach(sdf6)
quantreg25 <- rq(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                   PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                   dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                   UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                   UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                   UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                   main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                   PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc,tau = 0.25,data=sdf6WO2.log)
summary(quantreg25)
quantreg75 <- rq(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                   PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                   dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                   UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                   UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                   UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                   main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                   PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc,tau = 0.75,data=sdf6WO2.log)
summary(quantreg75)
anova(quantreg25,quantreg75) #p-value is smaller than 0.05

quantreg.all <- rq(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
                     PCIP51 + NUMBRANCH + PCIP14 + pct_white + PCIP52 + PCIP11 + 
                     dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
                     UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
                     UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
                     UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
                     main + C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
                     PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + NUMBRANCH:main + first_gen:md_faminc,tau=seq(0.05,0.95,by = 0.05),data = sdf6WO2.log) 
quantreg.plot <- summary(quantreg.all)
par(mfrow=c(1,1))
par(mar=c(0.1,0.1,0.1,0.1))
plot(quantreg.plot)
ks.test(normality,quantreg.all$residuals) #p-value = 0.03735 < 0.01, normal at 1% level.

######P4.Other Methods######
#Graphic Model by "huge"
library(huge)
sdf6.npn <- huge.npn(sdf6) #Nonparanormal
out.npn <- huge(sdf6.npn,lambda = c(0.05,0.4,0.95))
plot.huge(out.npn)

#linear mixed-effects models by "lmm"
par(mar=c(0.2,0.2,0.2,0.2))
plot(sdf6.log[1:5])
library(nlme)
lmm1 <- lme(md_earn_wne_p6 ~ pell_ever + veteran + pct_born_us + CONTROL + 
              PCIP51 + PCIP14 + pct_white + PCIP52 + PCIP11 + 
              dependent + female + HIGHDEG + median_hh_inc + PCIP47 + PCIP50 + 
              UGDS_NRA + md_faminc + age_entry + agege24 + PCIP22 + pct_ba + 
              UGDS_API + PREDDEG + UGDS_AIANOld + PCIP49 + PCIP15 + PCIP27 + 
              UGDS + UG25abv + CDR2 + UGDS_UNKN + UGDS_BLACKNH + married + 
              C150 + PCIP26 + loan_ever + PCIP40 + first_gen + poverty_rate + 
              PCIP48 + pct_hispanic - PCIP26 + poly(PCIP26,2) + first_gen:md_faminc, data=sdf6.log,random = ~1|main/NUMBRANCH)
summary(lmm1)
coef(lmm1)
coef(interlmWO2.log)

#hiearchical generalized linear model by "hglm"
library(hglm)
Y <- sdf6.log[,56] #log(md_earn_wne_6) as response
X <- as.matrix(sdf6.log[,c(3:55)])
z1 <- as.factor(sdf6.log[,1])
Z1 <- model.matrix(~ 0 + z1)
z2 <- as.factor(sdf6.log[,2])
Z2 <- model.matrix(~ 0 + z2)
hglm1 <- hglm(X = X, y = Y, Z = cbind(Z1, Z2),RandC = c(2, 20))
summary(hglm1)
plot(hglm1)

#Decision Tree by "tree"
library(tree)
attach(df6)
range(md_earn_wne_p6) #7300~117900
High <- ifelse(md_earn_wne_p6 > 50000,"Yes","No") 
newdf6 <- data.frame(df6,High)
newdf6 <- newdf6[,-56]

set.seed(2)
train <- sample(1:nrow(newdf6),nrow(newdf6)/2)
test <-  -train
training_data <-  newdf6[train,]
testing_data <- newdf6[test,]
testing_High <- High[test]

tree_model <- tree(High~.,training_data) #fit the tree model using training data
plot(tree_model)
text(tree_model,pretty=0,use.n=TRUE, all=TRUE, cex=.5)

tree_pred <- predict(tree_model,testing_data,type = "class") #check the how the tree is doing
mean(tree_pred !=testing_High) # misclassification error is only 0.01915991!

#Random Forest by "randomForest"
library(randomForest)
rf <- randomForest(md_earn_wne_p6~.,data=df6,mtry=4,ntree=500)
class(rf)
rf$importance