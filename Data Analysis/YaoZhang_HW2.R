pain <- read.csv("HW2Data2.csv")
dim(pain)

##1.Draw a boxplot for pain relief scores against different pain levels.
library(ggplot2)
ggplot(pain,aes(x=factor(PainLevel),y=Relief))+geom_boxplot()

##2.Perform a single factor ANOVA and compare the differences in “Relief Scores” of each “PainLevel” group.
##check normality
library(reshape)
pain.melt <- melt(pain)
group1 <- pain.melt[pain.melt$variable=="Relief",2]
qqnorm(group1)
qqline(group1)
##check equal variance
bartlett.test(value~variable,data=pain.melt)
##ANOVA
my.aov <- aov(PainLevel~Relief,data=pain)
summary(my.aov)

##3.Perform a two factor ANOVA with the following factors: Codeine and pain levels.
my.aovtwo <- aov(Relief~PainLevel*Codeine,data=pain)
summary(my.aovtwo)

##4.Report any significant effects from the two factor ANOVA.


##5.Use Scheffe’s method to conduct a multiple comparison for the different pairs of pain level.
library(sp)
library(agricolae)
agricolae::scheffe.test(my.aovtwo,"PainLevel",group=TRUE,console = TRUE)

##6. Based on the result from Scheffe’s method and the boxplot, discuss how to regroup different painlevels.