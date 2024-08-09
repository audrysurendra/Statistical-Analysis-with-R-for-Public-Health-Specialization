COPD <- read.csv('/Users/audrysurendra/Downloads/COPD_student_dataset.csv') 

# histogram
hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)

# outlier
subset(COPD, MWT1Best > 650)
subset(COPD, MWT1Best > 600 | MWT1Best < 150)

# FEV1
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1")   

# summary
list("Summary" = summary(COPD$MWT1Best), 
     "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), 
     "Range" = range(COPD$MWT1Best, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 

list("Summary" = summary(COPD$FEV1), 
     "Mean" = mean(COPD$FEV1, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), 
     "Range" = range(COPD$FEV1, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$FEV1, na.rm=TRUE))

# plot
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best") 

cor.test(COPD$FEV1, COPD$MWT1Best, use='complete.obs', method="pearson")
cor.test(COPD$FEV1, COPD$MWT1Best, use='complete.obs', method="spearman")


# regression model
MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1) 
confint(MWT1Best_FEV1)

par(mfrow=c(2,2)) 
plot(MWT1Best_FEV1)

# age
MWT1Best_AGE <- lm(MWT1Best~AGE, data=COPD) 
summary(MWT1Best_AGE) 
confint(MWT1Best_AGE) 

par(mfrow=c(2,2)) 
plot(MWT1Best_AGE)

# residuals
hist(residuals(MWT1Best_AGE), main = "Histogram of residuals", xlab = "Residuals") 

# multiple regression
MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD) 
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)

# walking distance and FVC 
MWT1Best_FVC <- lm(MWT1Best~FVC, data = COPD)
summary(MWT1Best_FVC)
confint(MWT1Best_FVC)

MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data = COPD) 
summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)


