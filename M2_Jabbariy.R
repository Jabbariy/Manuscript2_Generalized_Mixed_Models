library(aod)
library (readxl)
library(ggplot2)
library(multcomp)
library(lme4)
library(nlme)

setwd("/Users/yaz/Desktop/M2")
data<- read_excel("M2_data.xlsx")

data$Subject <- factor(data$Subject)
data$block <- as.integer(data$block)
data$trial <- as.integer(data$trial)
data$motion <- factor(data$motion,levels=c("V-","V+"))
data$landmark <- factor(data$landmark,levels=c("N","D","P"))
data$success <- factor(data$success)
data$pair <- with(data,interaction(landmark,motion,sep=""))

############### Success Rate ################

srlm <- glmer(success ~ landmark*motion + trial + (0+trial|Subject), data = data, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(srlm)

sror <- exp(cbind(OR=fixef(srlm),confint(srlm,parm="beta_",method="Wald"))) # OR for GLMM
sror

srpc <- glht(glm(success ~ pair, data=data, family="binomial"), linfct = mcp(pair = "Tukey")) # pairwise comparisons
summary(srpc)

#library (lsmeans)
#lsmeans :: lsmeans(srlm, pairwise~motion|landmark,adjust="tukey")
#library (lsmeans)
#lsmeans :: lsmeans(srlm, pairwise~landmark|motion,adjust="tukey")

sror2 <- exp(confint(srpc,parm="beta_")[["confint"]]) #OR and 95CI for pairwise comparisons
colnames(sror2) <- c("OR","2.5%","97.5%")
sror2

################ Route Retracing #################

rrlm <- lme(RR ~ landmark*motion + trial, random=~1+trial|Subject, data = data, control=lmeControl(opt='optim'))
summary(rrlm)

rrci <- intervals(rrlm, which = "fixed")
rrci

rrpc <- glht(glm(RR ~ pair, data = data), linfct = mcp(pair = "Tukey"))
summary(rrpc)

