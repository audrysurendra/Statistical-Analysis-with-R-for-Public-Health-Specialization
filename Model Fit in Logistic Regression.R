g <- read.csv('/Users/audrysurendra/Downloads/diabetes data for R.csv',header=TRUE, sep=',')

# design your logistic regression 
full_model <- glm(dm ~ g$age + g$chol + g$insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model)

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 
summary(null_model) 

# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 
R2

# c-statistic
install.packages("DescTools") 
require(DescTools) 
Cstat(full_model) 

# Hosmer-Lemeshow statistic and test
install.packages("ResourceSelection") 
require(ResourceSelection) 
full_model$y
# full_model$y  is the outcome variable we specified (dm); fitted(full_model) generates fitted values from the model

# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 

# there are different ways of plotting the information from a Hosmer-Lemeshow test
# Another way is to plot the ten ratios of observed:predicted cases, where a well-calibrated model would show ten points very near 1
install.packages("generalhoslem") 
require(generalhoslem) 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 

# analyse table of deviance 
anova(full_model, test = "Chisq")







