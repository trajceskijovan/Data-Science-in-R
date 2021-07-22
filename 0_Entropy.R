rm(list=ls())

##################################################################################
# Most direct way to extract UCI data
urldata <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroom <- read.table(url(urldata), header = FALSE, 
                       sep = ",", colClasses = "factor")

# A more complicated way to extract names
urlnames <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.names"
mess <- scan(urlnames, what="numeric", sep="\t")[70:102]
library(qdapRegex)
mess.names <- unlist(rm_between(mess,". ", ":", extract=TRUE))
mess.names <- mess.names[!is.na(mess.names)]

##################################################################################
# Old-fashioned way: Download data & Upload it
mushroom <- read.table("G:/My Drive/Teaching/Big_Data_Analytics/Data/agaricus-lepiota.data", 
                      header = FALSE, sep = ",")

# Simple way to assign column names
colnames(mushroom)  <- c("edible",
                       "capshape","capsurface","capcolor",
                       "bruises","odor",
                       "gillattachment","gillspacing","gillsize","gillcolor",
                       "stalkshape","stalkroot",
                       "stalksurfaceabovering","stalksurfacebelowring",
                       "stalkcolorabovering","stalkcolorbelowring",
                       "veiltype","veilcolor",
                       "ringnumber","ringtype",
                       "sporeprintcolor","population","habitat")
####################################################################################

# What is the sample size?
n <- nrow(mushroom)
k <- ncol(mushroom)-1 # one of the variables is target edible


df1 <- mushroom[mushroom$stalkroot != "?",]

mushroom <- subset(mushroom,stalkroot != "?") # What is the new sample size?
attach(mushroom)

# Entropy of parent (Figure 3.6)
class(edible)
y0       = as.numeric(edible)
n0       = length(y0)

y0.freq  = table(y0)
ype      = y0.freq[1]/n0
# ypp      = 1-ype
entropy0 = -ype*log2(ype)-(1-ype)*log2(1-ype)

# Child (Figure 3.7)
ngc      = nlevels(gillcolor) # m=12
gc.freq  = table(gillcolor, by=edible)
gc.entr = matrix(0,nrow=ngc,ncol=1) # Entropy container
for (i in 1:ngc){
  ni    = gc.freq[i,1]+gc.freq[i,2]
  gcpei = gc.freq[i,1]/ni
  gcppi = 1-gcpei
  gc.entr[i] = -gcpei*log2(gcpei) - gcppi*log2(gcppi) 
}


# Prepare the data for plot
rownames(gc.entr) = rownames(gc.freq)
colnames(gc.entr) = "Entropy"
gc.entr[is.nan(gc.entr)] = 0
# new data = gc.entr[is.nan(gc.entr)] eq.to subsetting the data
# versus
# gc.entr[is.nan(gc.entr)] = action

gc.prop = as.data.frame(rowSums(gc.freq)/n0) # p(cb)+p(ce)+ p(cy)=1
colnames(gc.prop) = "Proportion"

gc.data = cbind(gc.entr, gc.prop)
gc.ord  = gc.data[order(gc.data[,"Entropy"]),] 

# Name the variables
group      = rownames(gc.ord)
proportion = gc.ord[,2]
entropy    = gc.ord[,1]
gc.ord$cp  = cumsum(proportion) # cumulative sum of proportion (call it cp)

# Entropy Chart 3.7
# aes = aesthetics
library(ggplot2)
ggplot(gc.ord, aes(xmin=cp-proportion, xmax=cp, ymin=0, ymax=entropy)) + 
  geom_rect(aes(fill=group)) + 
  xlab("Proportion") + 
  ylab("Entropy")


