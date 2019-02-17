rm(list = ls())
setwd("~/R Workspace/HR Analytics")

hrdt = read.csv("SCMHRD BA Exp Learning Dataset 2019-10000 data.csv")
#View(hrdt)
names(hrdt)
attach(hrdt)
nrow(hrdt)
library(ggplot2)

ctCounty = as.data.frame(table(County))
#ctCounty
nrow(ctCounty)
which.max(ctCounty$Freq)
ctCounty = ctCounty[order(-ctCounty$Freq), ]
#1 - 10 Counties with highest number of Employees
dfc = head(ctCounty,10)
#No Point Plotting Histogram of cities, but still
p <- barplot(dfc$Freq, names.arg = dfc$County, ylim = c(0, 200))
text(x = p, y = dfc$Freq, label = dfc$Freq, pos = 3,cex = 0.8)

library(dplyr)
attr.yes = hrdt %>% filter(Attrition=='Yes'); dim(attr.yes)

attr.no = hrdt %>% filter(Attrition=='No'); dim(attr.no)

set.seed(1)
yes.train = sample(nrow(attr.yes), round(0.8*nrow(attr.yes)))#80% of Yes's taken in Train Set
no.train = sample(nrow(attr.no), round(0.8*nrow(attr.no)))#80% of No's taken in Train Set

attr.yes.train = attr.yes[yes.train, ]
attr.no.train = attr.no[no.train, ]

attr.yes.test = attr.yes[-yes.train, ]
attr.no.test = attr.no[-no.train, ]

# create the train and test sets
attr.train = rbind(attr.yes.train, attr.no.train)
attr.test = rbind(attr.yes.test, attr.no.test)

dim(attr.train); dim(attr.test)

attN = ifelse(attr.train$Attrition == "Yes", 1, 0)
attr.train = cbind(attr.train[, -2], attN)#Replacing Col Attrition, with Numeric Attn
#View(attr.train)
#Cleaning Data
attr.train$Last...Hike = as.numeric(gsub("[\\%,]", "", attr.train$Last...Hike))
attr.train$Gender = as.factor(attr.train$Gender)

#################          Kappa Function           #########################
getKappa = function(tab) {
  obs.f = sum(diag(tab))
  cat('Observed:', obs.f)
  
  exp.f = sum(diag(chisq.test(tab)$expected))
  cat('\nExpected:', exp.f)
  
  kappa = (obs.f - exp.f)/(sum(tab) - exp.f)
  cat('\n\tKappa is:', kappa)
}
###############################################################################
#Trying Logistic Regression'

attr.logr = glm(attN~Gender+poly(Salary, 3)+poly(Age.in.Company..Years., 3)+poly(Last...Hike,3)+poly(Last.Perf.Rating..1.4.,3), 
                data=attr.train,
                family=binomial)
attr.logr = step(attr.logr)
# don't run step()! because the tree model keeps all predictors!
summary(attr.logr)
attr.test$Last...Hike = as.numeric(gsub("[\\%,]", "", attr.test$Last...Hike))


pred.lr = predict(attr.logr, newdata=attr.test, type='response')
pred.lr = ifelse(pred.lr<0.17 , 0, 1)
ActualAttrition = ifelse(attr.test$Attrition == "Yes", 1, 0)
(tab.lr = table( pred.lr,ActualAttrition))


getKappa(tab.lr)
#Kappa is Very Poor
# Observed: 1431
# Expected: 1422.22
# Kappa is: 0.0151961

#------------------------------------------------------------------------------#
library(ROCR)  
library(pROC)
predob = prediction(pred.lr,ActualAttrition)

perf = performance(predob, "tpr", "fpr")

plot(perf, main="ROC")

(auc = performance(predob,"auc"))

plot(perf, main="AUC")
slotNames(auc)  
auc@y.name 
auc@y.values    

plot(auc(pred.lr,ActualAttrition))

###############################################################################

#Using Random Forest
# Random Forests

library(randomForest)

attr.rf1 = randomForest(attN~Gender+Salary+Age.in.Company..Years.+Last.Perf.Rating..1.4.+Last...Hike, 
                          data=attr.train, 
                          importance=T)
attr.rf1

names(attr.rf1)
plot(attr.rf1)
attr.rf1$importance

# plot the importance
#install.packages("rpart.plot")
library(rpart.plot)
library(randomForest)
varImpPlot(attr.rf1)
attN_TEST = ifelse(attr.test$Attrition == "Yes", 1, 0)
attr.test = cbind(attr.test[, -2], attN_TEST)#Replacing Col Attrition, with Numeric Attn
pred = predict(attr.rf1, newdata=attr.test)
(tab = table(ActualAttritionRF1, pred))



getKappa(tab)
################################################################################