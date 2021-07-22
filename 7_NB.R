rm(list=ls())
library(caret) 
data("iris") # Borrow data from caret
head(iris)

# In R Version 3.6.
# RNGkind(sample.kind = "Rounding")
set.seed(1)

ind = sample(seq(1,150,1),100,replace=FALSE)

xtrain = iris[ind,1:4]
ytrain = iris[ind,5]

xtest  = iris[-ind,1:4]
ytest  = iris[-ind,5]

library(e1071)
m1 = naiveBayes(x=xtrain,y=ytrain,laplace=1)
y1 = predict(m1,newdata=xtest)
table(y1,by=ytest)

m0 = naiveBayes(x=xtrain,y=ytrain,laplace=0)
y0 = predict(m0,newdata=xtest)
table(y0,by=ytest)