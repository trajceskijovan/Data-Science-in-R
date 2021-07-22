rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

# Required Packages
library(ROCR)     # For evaluation metrics
library(caret)    # For confusion matrix
library(kernlab)  # Another package for SVM
library(foreign)  # For importing data
library(e1071)    # SVM
library(C50)      # Tree


churn = read.arff("churn.arff")
colnames(churn)
# Drop "OVER_15MINS_CALLS_PER_MONTH", 
# Drop "REPORTED_SATISFACTION", 
# Drop "REPORTED_USAGE_LEVEL", 
# Drop "CONSIDERING_CHANGE_OF_PLAN"
k = c(7,9,10,11)
churn = churn[,-k] 

# Tranform factors to numeric
churn$LEAVE <- ifelse(churn$LEAVE == "LEAVE", 1, 0)
# churn$LEAVE <- ifelse(churn$LEAVE == "STAY", 0, 1)

churn$COLLEGE <- ifelse(churn$COLLEGE == "one", 1, 0)


# Choose a subset (6000 observations) to speed up the following analysis
set.seed(1)
i = sample(c(1:nrow(churn)),size=6000,replace = FALSE)
mydata = churn[i,]

# Set training and testing data
# Training (70%) and Testing set (30%)
j = sample(c(1:nrow(mydata)),size=4200,replace = FALSE)
traindata = mydata[j,]
testdata  = mydata[-j,]

# Model 1: Logistic regression
logit = glm(LEAVE ~ ., data = traindata, family = binomial(link="logit"))
summary(logit)

# Model 2a: Support Vector Machine
svm_model = svm(LEAVE ~ ., data= traindata, kernel="linear", cost =10) 
print(svm_model)
summary(svm_model)

# Model 2b: Support Vector Machine
svm_model2 = ksvm(LEAVE ~ ., data= traindata, kernel="vanilladot", cost =10) 
print(svm_model2)
summary(svm_model2)

# Model 3: Tree Model
mydata$LEAVE     = as.factor(mydata$LEAVE)
traindata$LEAVE   = as.factor(traindata$LEAVE)
tree.model      = C5.0(LEAVE ~., data=traindata, 
                       control = C5.0Control(minCases = 20))
tree.model
summary(tree.model)
plot(tree.model)

# Check what models are better then others
logistic_model = predict(logit, testdata, type = "response")
svm_predict    = predict(svm_model, testdata, type = "response")
svm2_predict   = predict(svm_model2, testdata, type = "response")
tree_predict   = predict(tree.model, testdata, type = "prob")[,2]

# The followings are all probability predictions
testdata$Yhat1 = logistic_model 
testdata$Yhat2 = svm_predict
testdata$Yhat3 = tree_predict

# Setting threshold parameters (Class prediction)
predict1 = function(x) ifelse(logistic_model > x, 1, 0)
predict2 = function(x) ifelse(svm_predict > x, 1, 0)
predict3 = function(x) ifelse(tree_predict > x, 1, 0)

hkbu = function (a,b,c){
  x=log(a)
  y=log(b)
  z=log(c)
  return(x+y+z)
}
hkbu(2,3,4)


# Functions(caret): confusionMatrix
library(caret)
# confusionMatrix(predict_values, actual_values)
x <- confusionMatrix(factor(predict1(0.5)), factor(testdata$LEAVE))
confusionMatrix(factor(predict2(0.5)), factor(testdata$LEAVE))
confusionMatrix(factor(predict3(0.5)), factor(testdata$LEAVE))

idx = seq(0.2,0.8,0.2)
y = data.frame()
for (i in idx){
  x = confusionMatrix(factor(predict1(i)), factor(testdata$LEAVE))
  xs = cbind(i,x$byClass["Sensitivity"])
  y = rbind(y,xs)
}


#Graph the results
library(ROCR) # Functions(ROCR): prediction, performance
predict_1 = prediction(testdata$Yhat1, testdata$LEAVE)
predict_2 = prediction(testdata$Yhat2, testdata$LEAVE)
predict_3 = prediction(testdata$Yhat3, testdata$LEAVE)

performance1 = performance(predict_1, "tpr", "fpr")
performance2 = performance(predict_2, "tpr", "fpr")
performance3 = performance(predict_3, "tpr", "fpr")

plot.new()
plot(performance1, col= "deeppink")
plot(performance2, add = TRUE, col= "cyan3")
plot(performance3, add = TRUE, col= "blueviolet")

abline(0,1, col = "red")
title("ROC curves")
legend(0.7, 0.5 ,c("Logistic", "SVM", "TREE"), 
       lty = c(1,1,1), 
       lwd = c(0.5,0.5,0.5),
       col = c("deeppink", "cyan3", "blueviolet"),
       ncol=1, cex=0.6, y.intersp=0.5)

# Accuracy
accuracy_log   = performance(predict_1, "acc")
accuracy_svm   = performance(predict_2, "acc")
accuracy_tree  = performance(predict_3, "acc")


#accuracy_log
#accuracy_svm
#accuracy_tree

# Accuracty Curves
plot(accuracy_log,main="Logistic Regression")
plot(accuracy_svm,main="Support Vector Machine")
plot(accuracy_tree,main="Decision Tree")


# AUC
auc_log    = performance(predict_1,"auc")
auc_log.y  = unlist(slot(auc_log,"y.values"))
auc_log.d  = round(auc_log.y,4)

auc_svm    = performance(predict_2,"auc")
auc_svm.y  = unlist(slot(auc_svm,"y.values"))
auc_svm.d  = round(auc_svm.y,4)

auc_tree   = performance(predict_3,"auc")
auc_tree.y = unlist(slot(auc_tree,"y.values"))
auc_tree.d = round(auc_tree.y,4)

auc = rbind(auc_log.d,auc_svm.d,auc_tree.d)
colnames(auc) = c("AUC")
rownames(auc)=c("Logistic","SVM","Tree")
auc


