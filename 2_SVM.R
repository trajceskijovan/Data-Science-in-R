rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots


# Simulate some data
set.seed(1)

# Assign y=-1 for the first 50 obs; Assign y=1 for the last 50 obs
# 100 numvers in this collector

y = c(rep(-1,50), rep(1,50) ) 

# For those y=-1, z1 and z2 follows N(0,1)
z1 = rnorm(50) # Standardized Income
z2 = rnorm(50) # Standardized Age
x1 = cbind(z1,z2)  

# For those y=+1 9y takes pos value) assume they follow , z1 and z2 follows N(0,1)+3
z1 = rnorm(50)+3 # Standardized Income
z2 = rnorm(50)+3 # Standardized Age
x2 = cbind(z1,z2) 

x  = rbind(x1,x2)

##########################################
# The above procedure can be simplified as follows:
# x = matrix(rnorm(200) , ncol =2) # Generate 2 variables with 50 observations each
# x[y==+1 ,]= x[y==1,] + 3
##########################################
# blue - negative group
# red - pos group

plot(x, col=3-y) # any error: Margin too small

dat = data.frame(x=x, y=as.factor(y)) # -1 and 1 as factor

library(e1071)
##########################################
## e1071 contains svm()
## y is the target
## ~ separate target and features
## . means all features are included
## kernel can be linear, polynomial, radial basis, sigmoid
## cost is the cost parameter
## When cost is small, margin will be wide (opposite to the argument in the note)
## and more support vectors.
## scale=FALSE tells the svm() function NOT to standardize the features to N(0,1)
##########################################

##########################################
# Huge cost parameter
# cost = penalty factor on the mis-classified obs
# Huge cost -> Hate errors -> Margin narrower -> No. of SV decreases
##########################################
# y tilda dot : all feature included
#data=df
svm10 = svm(y~. , data=dat , kernel ="linear", 
            cost=10, scale=FALSE )
plot(svm10, dat, 
     svSymbol=16, dataSymbol=1, # Check "pch in R" in Google
     color.palette = terrain.colors)
mtext("SVM - Cost=10", side=3) # subtitle
# svsymbol = 16 solid circle; datasymbol = hollow circle
# solid - support vec
# hollow - data

##########################################
# Crosses are support vectors
##########################################
svm10$index  # these obs are support vectors, these are 5 that determines decision boundary
summary(svm10) # 2 from minus group, 3 from pos. group
            
##########################################
# Small cost parameter
# Small cost -> Welcome errors -> Margin wider -> More SV
##########################################
svm1 =svm(y~., data=dat , kernel ="linear", 
          cost=0.1, scale =FALSE )
svm1$index
svm1$SV # call supp vectors

plot(svm1, dat, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("SVM - Cost=0.1", side=3)
svm1$index
summary(svm1)

##########################################
# Testing before selecting the model
##########################################

xtest = matrix(rnorm(20*2), ncol=2) # 20 #s in 2 col # generate 40 #s
ytest = sample(c(-1,1), 20, replace=TRUE) # Actual, sample from 2 numbers (1,-1) with replace on, and draw 20 times
xtest[ytest==1,] = xtest[ytest==1,] + 3 # if positive change original set plus 3
colnames(xtest) = c("z1","z2")

testdat = data.frame(x=xtest, y=as.factor(ytest))

ypred1 = predict(svm1, newdata=testdat)
table(ypred1, testdat$y)
table(predict=ypred1, truth=testdat$y)

ypred10 = predict(svm10, newdata=testdat)
table(predict=ypred10, truth= testdat$y)
# 2nd model almost same

##########################################
# Cross-Validation
##########################################
set.seed(1)
# tune function, try diff cost parameters on svm function
# i would like to apply svm function
tune.out=tune(svm ,
              y~.,
              data=dat ,
              kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 5,10,100) ))
summary (tune.out) # The optimal cost parameter is 0.1

bestmod =tune.out$best.model
summary (bestmod) # best is where cost par=o.1

ypred = predict(bestmod ,testdat)  # perfect prediction
table(predict=ypred , truth= testdat$y)

##########################################
# Nonlinear
##########################################
set.seed(1)
x = matrix(rnorm(200*2), ncol =2) # First 100 (train)
x[1:100 ,]   = x[1:100,]+2  # add first 100
x[101:150 ,] = x[101:150,]-2
y            = c( rep(-1,150), rep(1,50) )# 2 blue groups, 1 red group
dat          = data.frame(x=x, y=as.factor(y))
plot(x, col=3-y)

##########################################
# Small Cost, how to create non-lin. boundary?
##########################################
ind = sample(1:200, 100, replace=FALSE) #100 for test and 100 for train
train = dat[ind,] #ind rows, all columns
test  = dat[-ind,]

# library(e0171) # we already called the pack
svm.small = svm(y~., data=train, 
                kernel ="polynomial", degree=2, 
                cost=1)
plot(svm.small, train, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("Nonlinear with Cost=1", side=3)
svm.small$index
summary(svm.small)
ypred.small = predict(svm.small, newdata=test[,1:2])
acc.small = mean(as.numeric(ypred.small==test[,3])) #y attribute
#acc=79%

##########################################
# Large Cost
##########################################
svm.large = svm(y~., data=train, 
                kernel="polynomial", degree=2, 
                cost=1e5)
plot(svm.large, train, 
     svSymbol=16, dataSymbol=1,
     color.palette = terrain.colors)
mtext("Nonlinear with Cost=100000", side=3)
summary(svm.large)
ypred.large = predict(svm.large, newdata=test[,1:2])
acc.large = mean(as.numeric(ypred.large==test[,3]))


##########################################
# Cross-Validation
##########################################
set.seed(1)
tune.out = tune(svm, y~., data=dat[train,], kernel ="polynomial",
              ranges =list(cost=c(0.1, 1, 10, 100), degree=2))
summary(tune.out)

table(true=dat[-train,"y"], pred=predict (tune.out$best.model ,
                                           newdata=dat[-train ,]))
