library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(DMwR2)
library(rmda)
library(ggpubr)
library(ModelGood)
library(rms)
library(mRMRe)
library(DescTools)
library(Boruta)
library(sva)
library(e1071)
library(survcomp)
library(timeROC)
library(survival)

setwd("D:")
dt_train_pre<- read.csv('train.csv')
dt_test_pre <- read.csv('val.csv')
colnames(dt_test_pre) <- colnames(dt_train_pre)
#fix(dt_train_pre)
dt_adj_train <- dt_train_pre[,c(-1,-2)]
dt_adj_test <- dt_test_pre[,c(-1,-2)]


res.cox1<-coxph(Surv(TIME,Status)~.,data=dt_train_pre)
res.cox1<- step(res.cox1, direction = "backward")
summary(res.cox1)

cindex1 <- concordance.index(predict(res.cox1, newdata = dt_train_pre),surv.time = dt_train_pre$TIME,surv.event = dt_train_pre$Status,method="noether")
cindex1$c.index; cindex1$lower; cindex1$upper
pre1 <- predict(res.cox1, newdata = dt_train_pre)
pre1 <- data.frame(pre1)
#fix(pre1)
write.csv(pre1, 'pred-train.csv')
dt_train_pre <- bind_cols(dt_train_pre, pre1)
surroc <- timeROC(T = dt_train_pre$TIME,delta = dt_train_pre$Status,marker = dt_train_pre$pre1, cause=1,times=c(6,12,24,36,48),iid = TRUE)
surroc$AUC
confint(surroc,level = 0.95)$CI_AUC

cindex2 <- concordance.index(predict(res.cox1, newdata = dt_test_pre), surv.time = dt_test_pre$TIME,surv.event = dt_test_pre$Status,method="noether")
cindex2$c.index; cindex2$lower; cindex2$upper
pre2 <- predict(res.cox1, newdata = dt_test_pre)
pre2 <- data.frame(pre2)
#fix(pre2)
write.csv(pre2, 'pred-test.csv')
dt_test_pre <- bind_cols(dt_test_pre, pre2)
surroc2 <- timeROC(T = dt_test_pre$TIME,delta = dt_test_pre$Status,marker = dt_test_pre$pre2, cause=1,times=c(6,12,24,36,48),iid = TRUE)
surroc2$AUC

