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





