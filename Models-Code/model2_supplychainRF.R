library(cowplot)
library(ggplot2)
library(randomForest)


supply <- read.csv("SupplyChainData2.csv")
View(supply)
test_supply<-subset(supply,supply$Year == "2012")

print(head(test_supply))
nrow(test_supply)

# choose variables
test_supply <- test_supply[,-c(1,2,3,5,6,7)]

# if prescription trend =1, the prescription rate of that county is lower
# than the average prescription of all counties in specific year, we consider
# this situation is impact by some factor like policies.
test_supply[test_supply$Prescription_trend == 0,]$Prescription_trend<- "noneffective"
test_supply[test_supply$Prescription_trend == 1,]$Prescription_trend<- "effective"

test_supply <- as.data.frame(test_supply)

test_supply$Prescription_trend <- as.factor(test_supply$Prescription_trend)

test_supply$Population <- as.numeric(test_supply$Population)
str(test_supply)

# random forest
set.seed(42)
sp.imputed <-rfImpute(Prescription_trend ~.,data=test_supply,iter=6)
test_supply[!complete.cases(test_supply),]

# prints out the out of bag error rate, this should get smaller if the estimates
# are improving. since it does, we can conclude the estimates are goood as they 
# are going to get with this method
# build random forest model, but we specify data.imputed as the dataset
spmodel <- randomForest(Prescription_trend ~., data=sp.imputed, proximity=TRUE)
spmodel

#importance ranking 
importance(spmodel,type=1)
importance(spmodel,type=2)
varImpPlot(spmodel, color = "Blue")

# from the summary, 83.17% of the OOB samples were correctly classified 
# by the random forest
# there were 162 effective policies that were correctly classified as effective
# there were 100 non-effective policies that were correctly classified as non-effective
# create dataframe which include the information of error rate 

dim(spmodel$err.rate)
is.numeric(spmodel$err.rate)
sp.error.data <- data.frame(
  Trees=rep(1:nrow(spmodel$err.rate), times=3),
  Type=rep(c("OOB","Effective","NonEffective"), each=nrow(spmodel$err.rate)),
  Error=c(spmodel$err.rate[,"OOB"],
          spmodel$err.rate[,"effective"],
          spmodel$err.rate[,"noneffective"]))
ggplot(data=sp.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# the green line shows the error rate of classifying NonEffective policies
# the blue line shows overall out of bag rate
# the red line shows the error rate of classifying Effective policies
# In general, we see the error rates trend to stabilize after 100 trees and 
# classification of effective or non effective is stabilize after 350 trees too
# so we think 500 trees is enough for this hypothesis

# Now we need to make sure we are considering the optimal number of variables at
# each internal node in the tree
# we start by making an empty vector that can hold 10 values
oob.values <- vector(length=10)

# create a loop that tests different numbers of variables at each step
for(i in 1:10) {
  sp.model <- randomForest(Prescription_trend ~., data=sp.imputed, mtry=i, ntree=500)
  oob.values[i] <- sp.model$err.rate[nrow(sp.model$err.rate),1]
}
min(oob.values)
# The fourth value, corresponding to mtry = 9, which is the lowest OOB error rate
# so we change the default 3 to 9
dim(spmodel$err.rate)
is.numeric(spmodel$err.rate)
sp.error.data <- data.frame(
  Trees=rep(1:nrow(spmodel$err.rate), times=9),
  Type=rep(c("OOB","Effective","NonEffective"), each=nrow(spmodel$err.rate)),
  Error=c(spmodel$err.rate[,"OOB"],
          spmodel$err.rate[,"effective"],
          spmodel$err.rate[,"noneffective"]))
ggplot(data=sp.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# lastly, we use random forest to draw an MDS plot with samples
sp.distance.matrix <- dist(1-spmodel$proximity)
sp.mds.stuff <- cmdscale(sp.distance.matrix, eig=TRUE, x.ret = TRUE)

# then we calculate the percentage of variation in the distance matrix that
# X and Y axes account for
sp.mds.var.per <- round(sp.mds.stuff$eig/sum(sp.mds.stuff$eig)*100, 1)
sp.mds.values <- sp.mds.stuff$points
sp.mds.data <- data.frame(sample=rownames(sp.mds.values),
                       X=sp.mds.values[,1],
                       Y=sp.mds.values[,2],
                       Status=sp.imputed$Prescription_trend)

ggplot(data=sp.mds.data, aes(x=X, y=Y, label=sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", sp.mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", sp.mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# the X-axis accounts for 48% of the variation of the distance matrix
# the Y-axis accounts for 13.7% of the variation of the distance matrix
# the big difference occur when y = - 4
# lastly, if we got a trend of prescription 
# and didn't know it is effective but if they clustered at up left
# we can be confident that this trend is negative by some reason like policy

