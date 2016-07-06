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
