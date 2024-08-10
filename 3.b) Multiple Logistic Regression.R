g <- read.csv('/Users/audrysurendra/Downloads/diabetes data for R.csv',header=TRUE, sep=',')

# density plots
d <- density(g$age) 
plot(d,main = "") 

# multiple regression model
m <- glm(dm ~ g$age + g$gender + bmi, family=binomial (link=logit)) 
summary(m) 
exp(confint(m)) 

# model with these predictor variables: age, cholesterol and insurance type
insurance_type <- ifelse(g$insurance==1,"government",
                         ifelse(g$insurance==2,"private",0))

m2 <- glm(dm ~ g$age + g$chol + insurance_type, family=binomial (link=logit)) 
summary(m2) 
exp(confint(m2))
