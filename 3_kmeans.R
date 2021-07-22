rm(list=ls())

library(readr)
urlpage <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"
data <- read_csv(url(urlpage))
head(data)
# Attribute Information:
  
# 1) FRESH: annual spending (m.u.) on fresh products (Continuous);
# 2) MILK: annual spending (m.u.) on milk products (Continuous);
# 3) GROCERY: annual spending (m.u.)on grocery products (Continuous);
# 4) FROZEN: annual spending (m.u.)on frozen products (Continuous)
# 5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
# 6) DELICATESSEN: annual spending (m.u.) on and delicatessen products (Continuous);
# 7) CHANNEL: Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal)
# 8) REGION: Lisnon, Oporto or Other (Nominal) 


top.n.custs = function(data,cols,nm){
idx.to.remove = integer(0) # Empty bag
for (i in cols){
  morder = order(data[,i],decreasing = T) # Customer ID (Row number)
  idx = head(morder,nm) 
  idx.to.remove = union(idx.to.remove,idx) 
}
return(idx.to.remove)
}

top.custs = top.n.custs(as.data.frame(data),3:8,5)

length(top.custs)
the.top.customers = data[top.custs,]

other.customers = data[-top.custs,] # Our focus

# Start the Kmeans algorithm
# Same as below: k = kmeans(other.customers[,3:8], centers=5)
set.seed(1)
k = kmeans(other.customers[,-c(1,2)], centers=5)
k
attributes(k)
k$centers # Mean of 5 clusters
k$withinss # Within-sum-square of the 5 clusters
k$tot.withinss # Total within-sum-square (Sum of the above)

k$betweenss # totss-tot.withinss
k$totss # Before clustering, it compares all obs with the simple mean.

table(k$cluster)

# Which K should I use?
nk = 2:20 # Consider 19 k values
tries = 100
avg.totw.ss = matrix(0,19,1)

# Nested loops
for (ik in nk){
  totw.ss = matrix(0,nrow=tries,ncol=1)
  for (i in 1:tries){
    k.temp = kmeans(other.customers, centers=ik)
    totw.ss[i] = k.temp$tot.withinss # Error in our notes
  }
  avg.totw.ss[ik-1] = mean(totw.ss)
}


# Finding k using Elbow Method
plot(nk, avg.totw.ss,
     type="b", 
     main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# Data Visualization
clusters = as.factor(k$cluster)
library(ggplot2)
ggplot(other.customers,
       aes(x=Fresh,y=Grocery,color=clusters)) + 
  geom_point() + 
  ggtitle("Grocery vs Fresh")

ggplot(other.customers,aes(x= Fresh,y = Detergents_Paper,color = clusters)) + 
  geom_point() + 
  ggtitle("Detergents_Paper vs Fresh")

# Scaling 
# All numbers will be between 0 and 1
minmax = function(x){
  (x-min(x))/(max(x)-min(x))
}

normal = function(x){
  (x-mean(x))/sd(x)
}

df0 = other.customers[,-c(1,2)]
df1  = apply(df0,2,minmax)
df2  = apply(df0,2,normal)


ks1 = kmeans(df1, centers=5)
ks1$centers

