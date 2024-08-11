g <- read.csv('/Users/audrysurendra/Downloads/simulated HF mort data for GMPH.csv',header=TRUE, sep=',')

install.packages("survival")
require(survival)

# The proportionality assumption can be checked informally by plotting the hazards
# If the assumption is met then the hazard lines will be roughly parallel to each other
# note that that's only true when they're plotted on the log scale

cox.zph(fit, transform="km", global=TRUE)
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model
temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # plot the curves

# KM plot for gender
km_fit <- survfit(Surv(fu_time, death) ~ gender) 
# autoplot(km_fit)
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 

install.packages("survminer")
require(survminer)

# Deviance residuals are transformations of martingale residuals and help look for outliers or influential data points
res.cox <- coxph(Surv(fu_time, death) ~ g$age) 
ggcoxdiagnostics(res.cox, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age)) 
# Martingale residuals near 1 represent individuals that “died too soon”
# Large negative values correspond to individuals that “lived too long”

# checking assumption for COPD
cox.zph(fit, transform="km", global=TRUE)
fit <- coxph(Surv(fu_time, death) ~ g$copd) 
temp <- cox.zph(fit)
print(temp) 
plot(temp) 

# including an interaction term
fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function 
summary(fit) 
# This output agrees with the earlier approach and says that the interaction between gender and (transformed) time is not statistically significant
# i.e. there’s no apparent violation of the proportionality assumption

# If the assumption is violated, then one option is to include this interaction
# If the p-value is low but the hazards are proportional for most of the follow-up period, 
# then that suggests another solution: divide the survival analysis into two time periods





