g <- read.csv('/Users/audrysurendra/Downloads/simulated HF mort data for GMPH.csv',header=TRUE, sep=',')
getwd()
dim(g)
head(g)
g[1:5,]

install.packages("survival")
install.packages("ggplot")

library(survival) 
library(ggplot2) 

gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

# Kaplan-Meier plot
km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

# Kaplan-Meier estimates of the probability of survival over time
summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

# splitting the curve by gender:
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

# compare survival by gender
survdiff(Surv(fu_time, death) ~ gender, rho=0) 

# compare survival by age
age_group <- ifelse(g$age<65,0,1)
km_age_fit <- survfit(Surv(fu_time, death) ~ age_group) 
plot(km_age_fit)
survdiff(Surv(fu_time, death) ~ age_group, rho=0) 



