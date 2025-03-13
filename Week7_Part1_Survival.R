library("survival")
library("dplyr")

time <- c(3,7,16,25,32,44,49,50,55,72)
event <- c(1,1,1,1,1,1,1,1,1,1)

Ex_Surv <- Surv(time, event)
Ex_survfit <- survfit(Ex_Surv ~ 1)
plot(Ex_survfit, conf.int="none", mark.time=TRUE, xlab="Time (weeks)", ylab="Proportion Survival")

# Lecture Simple example 2
time <- c(3,7,16,25,32,44,49,50,55,72)
event <- c(1,0,1,1,0,1,1,0,1,1)
# Create a survival object
Ex_Surv <- Surv(time, event)
Ex_Surv

Ex_survfit <- survfit(Ex_Surv ~ 1)
plot(Ex_survfit, conf.int=TRUE, mark.time=TRUE, xlab="Time (weeks)", ylab="Proportion Survival")


# Acute Myelogenous Leukemia survival example - data included in the survival package

data <- data.frame(aml)
head(data)
AML_Surv <- Surv(aml$time, aml$status)
AML_survfit <- survfit(AML_Surv ~ aml$x)
plot(AML_survfit, conf.int=FALSE, mark.time=TRUE, xlab="Time", ylab="Proportion Survival")

survdiff(AML_Surv~x, data=aml)


melanoma <- boot::melanoma
melanoma <- melanoma%>%mutate(status=if_else(status == 2, 0, # "still alive"
                                             if_else(status == 1, 1, # "died of melanoma"
                                                     0)))
model <- coxph(Surv(time, status) ~ ., data = melanoma)
summary(model)

