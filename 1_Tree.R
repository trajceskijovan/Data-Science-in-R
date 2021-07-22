rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots

library(foreign)
churn = read.arff("churn.arff")
colnames(churn)
str(churn) # more info


# Drop "OVER_15MINS_CALLS_PER_MONTH", 
# Drop "REPORTED_SATISFACTION", 
# Drop "REPORTED_USAGE_LEVEL", 
# Drop "CONSIDERING_CHANGE_OF_PLAN"
k = c(7,9,10,11)
churn = churn[,-k] # run it once, be careful!

# Set 80% training and 20% testing data
set.seed(1) #same random number generated
a = seq(1,nrow(churn),by=1)
i = sample(a,nrow(churn)*0.8, replace=FALSE) #i has 16000 obs
train = churn[i,]
test  = churn[-i,]

n1 = sum(train$LEAVE=="LEAVE") #pick leave var, it is =1 or else 0# sum of leave value
n0 = 16000-n1

########################################################################
# Use C5.0 
# RWeka package can be used as well. However, Mac system cannot run J48()
########################################################################
library(C50)

vars = head(colnames(churn),-1) # X features names, deselect target

# Model 1
# Estimation supervised model, x features, x, y and 3rd is optional
mod1 = C5.0(x=train[,vars], y=train$LEAVE,      # train[,"LEAVE"], first blank include all rows
            control=C5.0Control(minCases =100)) # size of leaf/terminal node
mod1

out1 = summary(mod1) # more details
out1

# Plotting
library(partykit)
plot(mod1, main="Classification Tree (Model 1)", 
     type="simple",
     gp = gpar(fontsize=10))  #graphic choice

# Prediction (Testing data)
predict1 = predict(mod1, newdata = test[,vars], type="class")  #tree model, new data, , no need to include LEAVE
# we assume we ... we predict class; stay/leave
acc1     = mean(test$LEAVE==predict1) #Accuracy or ACC

# Prediction (Training data)
predict0 = predict(mod1, newdata = train[,vars], type="class")
acc0     = mean(train$LEAVE==predict0)  #accuracy or ACC
# Performance is slightly better in training data. 


# Feature Importance 
C5imp(mod1,metric="usage",pct=TRUE)

# Feature Importance->splits
C5imp(mod1,metric="splits")

# Model 2
# Estimation
# if mincases increases tree size decrease
mod2 = C5.0(x=train[,vars], y=train$LEAVE,
            control=C5.0Control(minCases=500))
summary(mod2) #based on traning data, error is higher

# Plotting
plot(mod2, main="Classification Tree (Model 2)", 
     type="extended",  #instread od SIMPLE
     gp = gpar(fontsize = 12))

# Prediction MOdel 2 lower accuracy acc2<aac1
predict2 = predict(mod2, newdata = test[,vars], type="class")
acc2     = mean(as.numeric(test[,"LEAVE"]==predict2))

# Feature Importance
C5imp(mod2,metric="usage",pct=TRUE)


