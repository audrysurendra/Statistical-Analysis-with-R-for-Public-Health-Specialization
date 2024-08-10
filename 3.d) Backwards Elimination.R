g <- read.csv('/Users/audrysurendra/Downloads/diabetes data for R.csv',header=TRUE, sep=',')

## Backwards Elimination

dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"]) # let's say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
ratio <- g[,'ratio'] 
location <- as.factor(g[,'location']) 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
frame <- as.factor(g[,'frame']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 
summary(model) 

anova(model, test = "Chisq")

# neither of the BP variables is significantly associated with the odds of being diagnosed with diabetes in this data set
# dropping the BP variables:  
model <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit)) 
summary(model) 
  
anova(model, test = "Chisq")

# coeffs of the remaining four variables have not changed much
# why is blood pressure not significant here despite what the literature says? 
# One way to find out is to see if it correlates with other variables:
cor.test(systolic, hdl) # not significant 
cor.test(systolic, bmi) # significant 
cor.test(systolic, chol) # very significant  
cor.test(systolic, age) # extremely significant   
  