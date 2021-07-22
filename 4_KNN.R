rm(list=ls())

library(caret) # knn3()
data("iris") # Borrow data from caret
data = iris

RNGkind(sample.kind = "Rounding")
set.seed(1)
ind = sample(seq(1,150,1),100,replace=FALSE)
xtraining = data[ind,1:4]
ytraining = data[ind,5]

set.seed(1)
ind1 = sample(seq(1,150,1),100,replace=FALSE)
xtesting  = data[-ind,1:4]
ytesting  = data[-ind,5]

# KNN
m1  = knn3(x=xtraining, y=ytraining, k=3)
m1
yhat1 = predict(m1,newdata=xtesting,type=c('class'))
mean(yhat1 == ytesting) # Proportion of Correct
mean(yhat1 != ytesting) # Incorrect
table(yhat1, ytesting)

m2  = knn3(xtraining, ytraining, k=5)
m2
yhat2 = predict(m2,newdata=xtesting,type=c('class'))
mean(yhat2 == ytesting)
table(yhat2, ytesting)

m3  = knn3(xtraining, ytraining, k=7)
m3
yhat3 = predict(m3,newdata=xtesting,type=c('class'))
mean(as.numeric(yhat3 == ytesting))
table(yhat3, ytesting)

m4  = knn3(xtraining, ytraining, k=29)
m4
yhat4 = predict(m4,newdata=xtesting,type=c('class'))
mean(as.numeric(yhat4 == ytesting))
table(yhat4, ytesting)

m5  = knn3(xtraining, ytraining, k=40)
m5
yhat5 = predict(m5,newdata=xtesting,type=c('class'))
mean(as.numeric(yhat5 == ytesting))
table(yhat5, ytesting)
