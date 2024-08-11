g <- read.csv('/Users/audrysurendra/Downloads/simulated HF mort data for GMPH.csv',header=TRUE, sep=',')

install.packages("survival")
library(survival)

install.packages("survminer")
library(survminer)

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) 
summary(cox)

# one coefficient for ethnicgroup
# R assumes that all variables are continuous

ethnicgroup <- factor(g[,"ethnicgroup"]) 
fu_time <- g[,"fu_time"]
death <- g[,"death"]
gender <- as.factor(g[,"gender"])

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox) # now get three coefficients

# Multiple Cox model
cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + g$prior_dnas + ethnicgroup)
summary(cox)

# Results of fitting a model with issues
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)  
# The coefficients and particularly the standard errors for quintile are all huge
# A couple of the standard errors for ethnic group are a bit high, but the main problem is quintile
# It seems to have infinitely wide CIs. Why?

table(quintile, exclude=NULL) 
# Only four patients have quintile zero
# This means invalid quintile, for instance when the postcode (zip code) can’t be mapped to 
# a small geographical area and therefore to a socio-economic status measure

# There’s another problem:
t <- table(quintile,death) 
t
round(100*prop.table(t,1),digits=1) # row %s 
# Of those four patients with quintile zero, no one died
# R chose the reference category by default, and it’s chosen quintile zero
# All the other five hazard ratios are relative to this tiny group of patients in which no one died
# It’s not surprising that the algorithm couldn’t come up with sensible HR estimates

# to fix this:
# 1) Change the reference category
quintile <- relevel(quintile, ref = 2) # quintile 1 as the ref cat again

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox)

# 2) Combine categories
quintile_5groups <- g[,'quintile'] 
quintile_5groups[quintile_5groups==0] <- 5 
quintile_5groups <- factor(quintile_5groups) 
table(quintile_5groups, exclude=NULL) 

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 
summary(cox) 

# 3) Exclude the patients
quintile_5groups <- g[,'quintile'] 
quintile_5groups[quintile_5groups==0] <- NA # set the zeroes to missing 
quintile_5groups <- factor(quintile_5groups) 
table(quintile_5groups, exclude=NULL) 
quintile_5groups 

cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup) 
summary(cox)

# 4) Drop the offending variable
cox <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup) # don't put quintile in the model
summary(cox)



