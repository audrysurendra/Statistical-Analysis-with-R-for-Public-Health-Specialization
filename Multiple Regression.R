COPD <- read.csv('/Users/audrysurendra/Downloads/COPD_student_dataset.csv') 

# dimensions
dim(COPD) 
head(COPD) 

# VARIBALES

# age
class(COPD$AGE)
summary(COPD$AGE) 
par(mfrow=c(1,1)) 
hist(COPD$AGE) 

# cat score
class(COPD$CAT)
summary(COPD$CAT) 
hist(COPD$CAT) 

# severity
class(COPD$COPDSEVERITY) 
table(COPD$COPDSEVERITY, exclude = NULL) 

# gender
class(COPD$gender) 
COPD$gender <- as.factor(COPD$gender) 
class(COPD$gender)
table(COPD$gender, exclude = NULL) 

# copd
class(COPD$copd) 
COPD$copd <- as.factor(COPD$copd) 
class(COPD$copd)
str(COPD$copd)

# linear regression
lr1 <- lm(MWT1Best~copd, data=COPD)
summary(lr1)

# relevelling; NOTE: this function only works with variables saved as factors
COPD$copd <- relevel(COPD$copd, ref=3) 
lr1 <- lm(MWT1Best~copd, data=COPD)
summary(lr1)

# Create new variables from old ones 
comorbid <- length(COPD$Diabetes) 
comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 | COPD$AtrialFib == 1 | COPD$IHD == 1 ] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid)
COPD$comorbid <- comorbid

# install.packages("Hmisc")
# describe(COPD)

install.packages("gmodels")
library(gmodels)
CrossTable(COPD$copd)

# correlation matrix
my_data <- COPD[c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(my_data)
cor_matrix
round(cor_matrix,2)

# correlation plot
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data=COPD)

# examine associations between categorical variables
CrossTable(COPD$hypertension, COPD$IHD)

# examine collinearity
install.packages("mctest")
library(mctest)
#imcdiag(model.matrix(mlr1)[,-1],m1r1$model[1],method="VIF")

# interactions between binary variables
COPD$Diabetes <- c(0,1)[as.integer(COPD$Diabetes)]
COPD$AtrialFib <- c(0,1)[as.integer(COPD$AtrialFib)]
DAF <- COPD$Diabetes * COPD$AtrialFib
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=COPD) 
summary(r1) 
confint(r1)









