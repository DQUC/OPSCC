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

setwd("D:/")
dt_train_pre1 <- read_csv('pred_train_DL.csv')
dt_test_pre1 <- read_csv('pred_val_DL.csv')
dt_train_pre2 <- read_csv('pred_train_DLCS.csv')
dt_test_pre2 <- read_csv('pred_val_DLCS.csv')
dt_train_pre3 <- read_csv('pred_train_Clinical.csv')
dt_test_pre3 <- read_csv('pred_val_Clinical.csv')
dt_train_pre4 <- read_csv('pred_train_AJCC.csv')
dt_test_pre4 <- read_csv('pred_val_AJCC.csv')
dt_train_pre5 <- read_csv('pred_train_HPV.csv')
dt_test_pre5 <- read_csv('pred_val_HPV.csv')
dt_train_pre6 <- read_csv('pred_train_T.csv')
dt_test_pre6 <- read_csv('pred_val_T.csv')

# Calibration plot
#DL
units(dt_train_pre1$TIME) <- "Month"
dd=datadist(dt_train_pre1)
options(datadist="dd")
f1 <- cph(Surv(dt_train_pre1$TIME,dt_train_pre1$Status==1)~pre1, data=dt_train_pre1, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal1 <- calibrate(f1, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre1$TIME) <- "Month"
dd=datadist(dt_test_pre1)
options(datadist="dd")
f2 <- cph(Surv(dt_test_pre1$TIME,dt_test_pre1$Status==1)~pre2, data=dt_test_pre1, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal2 <- calibrate(f2, cmethod = "KM", method="boot",u=36,m=15,B=1000)
#DLCS
units(dt_train_pre2$TIME) <- "Month"
dd=datadist(dt_train_pre2)
options(datadist="dd")
f3 <- cph(Surv(dt_train_pre2$TIME,dt_train_pre2$Status==1)~pre3, data=dt_train_pre2, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal3 <- calibrate(f3, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre2$TIME) <- "Month"
dd=datadist(dt_test_pre2)
options(datadist="dd")
f4 <- cph(Surv(dt_test_pre2$TIME,dt_test_pre2$Status==1)~pre4, data=dt_test_pre2, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal4 <- calibrate(f4, cmethod = "KM", method="boot",u=36,m=15,B=1000)
#clinical
units(dt_train_pre3$TIME) <- "Month"
dd=datadist(dt_train_pre3)
options(datadist="dd")
f5 <- cph(Surv(dt_train_pre3$TIME,dt_train_pre3$Status==1)~pre5, data=dt_train_pre3, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal5 <- calibrate(f5, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre3$TIME) <- "Month"
dd=datadist(dt_test_pre3)
options(datadist="dd")
f6 <- cph(Surv(dt_test_pre3$TIME,dt_test_pre3$Status==1)~pre6, data=dt_test_pre3, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal6 <- calibrate(f6, cmethod = "KM", method="boot",u=36,m=15,B=1000)
#AJCC
units(dt_train_pre4$TIME) <- "Month"
dd=datadist(dt_train_pre4)
options(datadist="dd")
f7 <- cph(Surv(dt_train_pre4$TIME,dt_train_pre4$Status==1)~pre7, data=dt_train_pre4, x=TRUE, y=TRUE, surv = TRUE, time.inc =36)
cal7 <- calibrate(f7, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre4$TIME) <- "Month"
dd=datadist(dt_test_pre4)
options(datadist="dd")
f8 <- cph(Surv(dt_test_pre4$TIME,dt_test_pre4$Status==1)~pre8, data=dt_test_pre4, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal8 <- calibrate(f8, cmethod = "KM", method="boot",u=36,m=15,B=1000)


#HPV
units(dt_train_pre5$TIME) <- "Month"
dd=datadist(dt_train_pre5)
options(datadist="dd")
f9 <- cph(Surv(dt_train_pre5$TIME,dt_train_pre5$Status==1)~pre9, data=dt_train_pre5, x=TRUE, y=TRUE, surv = TRUE, time.inc =36)
cal9 <- calibrate(f9, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre5$TIME) <- "Month"
dd=datadist(dt_test_pre5)
options(datadist="dd")
f10 <- cph(Surv(dt_test_pre5$TIME,dt_test_pre5$Status==1)~pre10, data=dt_test_pre5, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal10 <- calibrate(f10, cmethod = "KM", method="boot",u=36,m=15,B=1000)

#T stage
units(dt_train_pre6$TIME) <- "Month"
dd=datadist(dt_train_pre6)
options(datadist="dd")
f11 <- cph(Surv(dt_train_pre6$TIME,dt_train_pre6$Status==1)~pre11, data=dt_train_pre6, x=TRUE, y=TRUE, surv = TRUE, time.inc =36)
cal11 <- calibrate(f11, cmethod = "KM", method="boot",u=36,m=40,B=1000)

units(dt_test_pre6$TIME) <- "Month"
dd=datadist(dt_test_pre6)
options(datadist="dd")
f12 <- cph(Surv(dt_test_pre6$TIME,dt_test_pre6$Status==1)~pre12, data=dt_test_pre6, x=TRUE, y=TRUE, surv = TRUE, time.inc = 36)
cal12 <- calibrate(f12, cmethod = "KM", method="boot",u=36,m=15,B=1000)

#校准曲线训练集
opar <- par(no.readonly=TRUE)
par(mfrow = c(1, 2))

plot(cal1, errbar.col = "Yellow",lwd = 2,lty=2, cex.axis =1.2,
     cex.lab = 1.2,xlab="Model predicted survival probability", ylab="Observed survival (probability)",
     xlim = c(0,1),ylim = c(0,1), subtitles = FALSE)
lines(cal1[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "skyblue",pch = 16)
par(new=TRUE)
lines(cal3[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "darkorange",pch = 16)
par(new=TRUE)
lines(cal5[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "orchid",pch = 16)
par(new=TRUE)
lines(cal7[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "seagreen",pch = 16)
par(new=TRUE)
lines(cal9[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "red",pch = 16)
par(new=TRUE)
lines(cal11[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "green",pch = 16)
par(new=TRUE)
legend("topleft", c("DL","DLCS","Clinical model","AJCC stage","HPV","T stage"),
       lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2), col = c("skyblue","darkorange","orchid","seagreen","red","green"), bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

#校准曲线验证集
plot(cal2, errbar.col = "Yellow",lwd = 2,lty=2, cex.axis =1.2,
     cex.lab = 1.2,xlab="Model predicted survival probability", ylab="Observed survival (probability)",
     xlim = c(0,1),ylim = c(0,1), subtitles = FALSE)
lines(cal2[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "skyblue",pch = 16)
par(new=TRUE)
lines(cal4[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "darkorange",pch = 16)
par(new=TRUE)
lines(cal6[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "orchid",pch = 16)
par(new=TRUE)
lines(cal8[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "seagreen",pch = 16)
par(new=TRUE)
lines(cal10[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "red",pch = 16)
par(new=TRUE)
lines(cal12[,c("mean.predicted","KM")],type = "b",lwd = 2,col = "green",pch = 16)
par(new=TRUE)
legend("topleft", c("DL","DLCS","Clinical model","AJCC stage","HPV","T stage"),
       lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2), col = c("skyblue","darkorange","orchid","seagreen","red","green"), bty = "n")
abline(0,1,col="black",lty=2,lwd=1)

#列线图
surv <- Survival(f3)
nom <- nomogram(f3, fun = list(function(x) surv(24,x),function(x) surv(36,x)), fun.at = c(0.01, 0.2, 0.5,0.8,0.9,.99), funlabel = c("1-years Survivsl Probability","3-years Survivsl Probability"), lp = F)
plot(nom)

