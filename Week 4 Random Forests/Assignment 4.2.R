setwd('/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6020_Predictive Analytics/Week 4 Random Forests/SouthGermanCredit')


# Data Preperation
sgc <- read.csv('SouthGermanCredit.csv')
head(sgc)
colnames(sgc) <- c(
  "status", "duration", "credit_history", "purpose", "amount", 
  "savings", "employment_duration", "installment_rate",
  "personal_status_sex", "other_debtors",
  "present_residence", "property",
  "age", "other_installment_plans",
  "housing", "number_credits",
  "job", "people_liable", "telephone", "foreign_worker",
  "credit_risk"
)
head(sgc)

str(sgc)
levels(sgc$credit_risk) <- c("bad", "good")
levels(sgc$status) = c("no checking account",
                       "... < 0 DM",
                       "0<= ... < 200 DM",
                       "... >= 200 DM / salary for at least 1 year")
levels(sgc$credit_history) <- c(
  "delay in paying off in the past",
  "critical account/other credits elsewhere",
  "no credits taken/all credits paid back duly",
  "existing credits paid back duly till now",
  "all credits at this bank paid back duly")
levels(sgc$purpose) <- c(
  "others",
  "car (new)",
  "car (used)",
  "furniture/equipment",
  "radio/television",
  "domestic appliances",
  "repairs",
  "education", 
  "vacation",
  "retraining",
  "business")
levels(sgc$savings) <- c("unknown/no savings account",
                         "... <  100 DM", 
                         "100 <= ... <  500 DM",
                         "500 <= ... < 1000 DM", 
                         "... >= 1000 DM")
levels(sgc$employment_duration) <- 
  c(  "unemployed", 
      "< 1 yr", 
      "1 <= ... < 4 yrs",
      "4 <= ... < 7 yrs", 
      ">= 7 yrs")
sgc$installment_rate <- ordered(sgc$installment_rate)
levels(sgc$installment_rate) <- c(">= 35", 
                                  "25 <= ... < 35",
                                  "20 <= ... < 25", 
                                  "< 20")
levels(sgc$other_debtors) <- c(
  "none",
  "co-applicant",
  "guarantor"
)
levels(sgc$personal_status_sex) <- c(
  "male : divorced/separated",
  "female : non-single or male : single",
  "male : married/widowed",
  "female : single")
sgc$present_residence <- ordered(sgc$present_residence)
levels(sgc$present_residence) <- c("< 1 yr", 
                                   "1 <= ... < 4 yrs", 
                                   "4 <= ... < 7 yrs", 
                                   ">= 7 yrs")
levels(sgc$property) <- c(
  "unknown / no property",
  "car or other",
  "building soc. savings agr./life insurance", 
  "real estate"
)
levels(sgc$other_installment_plans) <- c(
  "bank",
  "stores",
  "none"
)
levels(sgc$housing) <- c("for free", "rent", "own")
sgc$number_credits <- ordered(sgc$number_credits)
levels(sgc$number_credits) <- c("1", "2-3", "4-5", ">= 6")
levels(sgc$job) <- c(
  "unemployed/unskilled - non-resident",
  "unskilled - resident",
  "skilled employee/official",
  "manager/self-empl./highly qualif. employee"
)
levels(sgc$people_liable) <- c("3 or more", "0 to 2")
levels(sgc$telephone) <- c("no", "yes (under customer name)")
levels(sgc$foreign_worker) <- c("yes", "no")

str(sgc)
sgc$credit_risk <- as.factor(sgc$credit_risk)
str(sgc$credit_risk)


# Apply randomForest() function
library(randomForest)
library(MASS)
set.seed(17)
sgc.rf <- randomForest(credit_risk ~ ., data = sgc,
                       mtry = 2, importance = TRUE,
                       do.trace = 100)
print(sgc.rf)


# Model Comparison with Errorest Functions
library(ipred)
set.seed(131)
error.RF <- numeric(10)
for (i in 1:10) error.RF[i] <-
  errorest(credit_risk ~ ., data = sgc,
           model = randomForest, mtry = 2)$error
summary(error.RF)  

library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) error.SVM[i] <-
  errorest(credit_risk ~ ., data = sgc,
           model = svm, cost = 10, gamma = 1.5)$error
summary(error.SVM)


par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(sort(sgc.rf$importance[,i], dec = TRUE),
       type = "h", main = paste("Measure", i))
}

