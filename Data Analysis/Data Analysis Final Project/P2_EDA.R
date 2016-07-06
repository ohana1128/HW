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

