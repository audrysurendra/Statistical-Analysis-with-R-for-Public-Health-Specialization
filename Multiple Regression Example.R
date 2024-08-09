COPD <- read.csv('/Users/audrysurendra/Downloads/COPD_student_dataset.csv') 

# we are interested in the effect of age (AGE), lung function (FEV1), gender (gender) and smoking status (smoking) 
# on walking distance (MWT1best) in COPD patients

# To fit such a model in R we issue the following code: 
mlr1 <- lm(MWT1Best~AGE+FEV1+factor(gender)+factor(smoking), data=COPD) 
summary(mlr1) 
confint(mlr1) 
# gender = 0 if female and 1 if male; 
# smoking = 1 if current and 2 if ex

# interested in the change from being an ‘ex’ smoker to a ‘current’ smoker
# In terms of the model fitted, need to change the reference category:
COPD$smoking <- relevel(factor(COPD$smoking), ref = 2) 
mlr1 <- lm(MWT1Best~AGE+FEV1+factor(gender)+factor(smoking), data=COPD) 
summary(mlr1) 
confint(mlr1) 

# recode so that current smokers are coded as 0 and ex-smokers are coded as 1
COPD$smoking <- as.integer(COPD$smoking)
COPD$smoking[COPD$smoking == 2] <- 0 

# check if the effect of lung function on walking distance depends on smoking status
# include an interaction between FEV1 and smoking: 
mlr2 <- lm(MWT1Best~AGE+factor(gender)+(FEV1*factor(smoking)), data=COPD) 
summary(mlr2) 
confint(mlr2)

# no evidence to support the hypothesis of an interaction between FEV1 and smoking 
# The p-value equals 0.686 
# unlikely to proceed with this model. 

