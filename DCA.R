#library(dcurves)
library(survival)
source("D:/dca.R")
source("D:/stdca.R")
df_surv <- read.csv("D:/",header = T)

dim(df_surv)
str(df_surv)

Model1 <- coxph(Surv(TIME, Status) ~ Model1, 
                  data = df_surv)
Model2 <- coxph(Surv(TIME, Status) ~ Model2, data = df_surv)
Model3<- coxph(Surv(TIME, Status) ~ Model3, data = df_surv)
Model4<- coxph(Surv(TIME, Status) ~ Model4, data = df_surv)
Model5<- coxph(Surv(TIME, Status) ~ Model5, data = df_surv)
Model6<- coxph(Surv(TIME, Status) ~ Model6, data = df_surv)

df_surv$Model1 <- c(1-(summary(survfit(Model1, newdata=df_surv), times=36)$surv))
df_surv$Model2 <- c(1-(summary(survfit(Model2, newdata=df_surv), times=36)$surv))
df_surv$Model3 <- c(1-(summary(survfit(Model3, newdata=df_surv), times=36)$surv))
df_surv$Model4 <- c(1-(summary(survfit(Model4, newdata=df_surv), times=36)$surv))
df_surv$Model5 <- c(1-(summary(survfit(Model5, newdata=df_surv), times=36)$surv))
df_surv$Model6 <- c(1-(summary(survfit(Model6, newdata=df_surv), times=36)$surv))


stdca(data=df_surv, 
      outcome="Status", 
      ttoutcome="TIME", 
      timepoint=36, 
      predictors=c("Model1","Model2","Model3","Model4","Model5","Model6"),  
      smooth=TRUE
)

