g <- read.csv('/Users/audrysurendra/Downloads/diabetes data for R.csv',header=TRUE, sep=',')
dim(g)
colnames(g)              
dimnames(g)[[2]]

# insurance: 0=none, 1=government, 2=private
# fh = family history of diabetes (yes/no, where 1=yes, 0=no)
# smoking: 1=current, 2=never and 3=ex

chol <- g$chol
gender <- as.factor(g$gender) 
dm <- as.factor(g$dm)

t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) # get proportions rounded to 3dp
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

# to see missing values
dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

summary(chol)

height <- g$height
weight <- g$weight
summary(height)
summary(weight)

# height is in inches and weight is in pounds
# convert to SI units, kgs and meters
height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)

# we want groups for underweight [<18.5], normal [18.5-25], overweight [>25] and obese [>30]
bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 

# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 
dm_by_bmi_category 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
# margin = 1 specifies that the table gives the row percentages, 
# i.e. the frequencies of each column (diabetes status) in each row (BMI category)
# margin = 2, would display the inverse, i.e. the frequencies of each row in each column

# Make age groups from age, allowing for any missing values; under 45, 45-64, 65-74 and 75 or over
age_groups <- ifelse(g$age<45, "under 45",
                     ifelse(g$age>45 & g$age<64, "45-64",
                            ifelse(g$age>65 & g$age<74, "65-74",
                                   ifelse(g$age>75,"over 75",NA))))

# tabulate age group by itself, followed by a cross-tabulation with gender
table(age_groups, exclude = NULL) 
age_by_gender <- table(age_groups, g$gender, exclude = NULL) 

# add the overall percentages to this cross-tab  
round(100 * prop.table(age_by_gender), digits = 1) 


# Simple logistic regression: how to run a model with only one predictor
m <- glm(dm ~ 1, family=binomial (link=logit))
# The “1” is just R’s way of saying that there’s only an intercept term in the model
summary(m)

# check how R has interpreted the binary outcome “dm”
table(m$y)
# R is modelling the log odds of dm=1 and not the log odds of dm=0

# The next simplest model is one with one predictor
m <- glm(dm ~ g$gender, family=binomial (link=logit))
summary(m)
# This means we are saying that the log odds of having diabetes differs by gender alone

# Include a continuous variable in the model instead:
m <- glm(dm ~ g$age, family=binomial (link=logit))
summary(m)

# is it reasonable to assume that the relation between age and the log odds of having diabetes is linear?
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(g$age, g$dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 

# model coefficients
m$coefficients 
exp(m$coefficients)

# Of those with a recorded diabetes status, what percentage of people from Buckingham have diabetes?
diabetes_by_location <- table(g$location, g$dm, exclude = NULL) 
round(100 * prop.table(diabetes_by_location, margin=1), digits = 1) 

# Fit a logistic regression with “location” as the predictor variable
# What are the log odds of having diabetes being from Louisa compared with Buckingham?  
m2 <- glm(dm ~ g$location, family=binomial (link=logit))
m2

