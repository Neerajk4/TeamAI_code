library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(randomForest)
library(caret)

setwd("R files/practice files/accenture/datasets/model/County")
mydata <- read.csv("combined_prescription.csv", stringsAsFactors = FALSE)

colnames(mydata) <- c("x","State", "Year", "policy1","policy2","policy3","policy4","policy5","policy6", "policy7",
                      "policy8", "policy9", "policy10", "policy11", "policy12", "policy13", "policy14", "policy15", "policy16", "policy17", "policy18",
                      "policy19", "key2", "Population", "percent_male", "percent_female", "under_18", "X18.24", "X25.44", "X45.64", "over_65", "Mean_income", 
                      "Unemployment_rate", "County", "FIPS.County.Code", "Prescribing.Rate", "outcome1", "outcome2", "outcome3")

mydata <- mydata[,-c(1,23,34,36,37,39)]
mydata[,22:31] <- scale(mydata[,22:31])

##one hot encoding of State field
aaa <- select(mydata, State) %>% unique()
State1 <- c(1:10)
ccc <- cbind(aaa, State1)
mydata <- merge(mydata, ccc, by = "State")

## Calculating propensity scores (optional)
ps.1 <- glm(policy11 ~ percent_male + percent_female + under_18 + X18.24 + X25.44 + X45.64 + over_65 + Mean_income + Unemployment_rate, 
            data = mydata, family=binomial(link="logit"), na.action=na.pass)

ps.2 <- glm(policy13 ~ percent_male + percent_female + under_18 + X18.24 + X25.44 + X45.64 + over_65 + Mean_income + Unemployment_rate, 
            data = mydata, family=binomial(link="logit"), na.action=na.pass)

ps.3 <- glm(policy16 ~ percent_male + percent_female + under_18 + X18.24 + X25.44 + X45.64 + over_65 + Mean_income + Unemployment_rate, 
            data = mydata, family=binomial(link="logit"), na.action=na.pass)

ps.4 <- glm(policy17 ~ percent_male + percent_female + under_18 + X18.24 + X25.44 + X45.64 + over_65 + Mean_income + Unemployment_rate, 
            data = mydata, family=binomial(link="logit"), na.action=na.pass)

ps.5 <- glm(policy18 ~ percent_male + percent_female + under_18 + X18.24 + X25.44 + X45.64 + over_65 + Mean_income + Unemployment_rate, 
            data = mydata, family=binomial(link="logit"), na.action=na.pass)

mydata$pscore1 <- predict(ps.1, newdata = mydata, type = "response")
mydata$pscore2 <- predict(ps.2, newdata = mydata, type = "response")
mydata$pscore3 <- predict(ps.3, newdata = mydata, type = "response")
mydata$pscore4 <- predict(ps.4, newdata = mydata, type = "response")
mydata$pscore5 <- predict(ps.5, newdata = mydata, type = "response")


##subsetting columns and splitting into test and training sets.
set.seed(1234567)
# split data 70/30
train_sub = sample(nrow(mydata),7/10*nrow(mydata))
train_data = mydata[train_sub,]
test_data = mydata[-train_sub,]

## Random Forest model

test.rf2 <- randomForest(formula = train_data$outcome2 ~. , data = train_data, mtry = 6, ntree=900)
test.rf <- randomForest(formula = as.factor(train_data$outcome2) ~. , data = train_data, mtry = 6, ntree=900)


test.rf2 <- randomForest(train_data$outcome2 ~ Year + State1 + policy1 + policy2 + policy3 + policy4 + policy5 + policy6 + policy7 + policy8
+ policy9 + policy10 + policy11 + policy12 + policy13 + policy14 + policy15 + policy16 + policy17 
+ policy18 + Population + percent_male + under_18 + X18.24 + X25.44 + X45.64 + over_65 
+ Mean_income + Unemployment_rate + policy16*policy18 + policy16*Unemployment_rate, data = train_data, mtry = 6, ntree = 900)

test.rf <- randomForest(as.factor(train_data$outcome2) ~ Year + State1 + policy1 + policy2 + policy3 + policy4 + policy5 + policy6 + policy7 + policy8
                         + policy9 + policy10 + policy11 + policy12 + policy13 + policy14 + policy15 + policy16 + policy17 
                         + policy18 + Population + percent_male + under_18 + X18.24 + X25.44 + X45.64 + over_65 
                         + Mean_income + Unemployment_rate + policy16*policy18 + policy16*Unemployment_rate, 
                        data = train_data, mtry = 6, ntree = 900)

##combining predictions with testing set
test_data$prob <- predict(test.rf2, test_data, type = "response")
test_data$prediction <- predict(test.rf, test_data, type = "response")
##this is confusion matrix
con_mat <- table(test_data$prediction, test_data$outcome2)


##Generate out of bag error
plot(test.rf, legend = TRUE)
varImpPlot(test.rf,  sort = T, n.var=31, main="Top 20 - Variable Importance")


oobData = as.data.frame(plot(test.rf))
ntree <- c(1:nrow(oobData))
oobData <- cbind(ntree, oobData)
oobData2 <- gather(oobData, "type", "error", -ntree)
ggplot(data = oobData2, aes(x = ntree, y = error, color = type)) + geom_line()

##Analysis of results
roc(test_data$outcome2, test_data$prob, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",col="#377eb8", lwd=4,
    print.auc=TRUE)


ROC1 <- roc(test_data$outcome2, test_data$prob)
AUC1 <- as.numeric(auc(ROC1))
accuracy <- (con_mat[1,1] + con_mat[2,2])/ (nrow(test_data))
precision <- con_mat[1,1]/ (con_mat[1,1] + con_mat[1,2])
recall<- con_mat[1,1]/ (con_mat[1,1] + con_mat[2,1])

## parameter tuning using caret packages.  Note it takes a long time to run
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=6)
rf_gridsearch <- train(as.factor(outcome2)~., data=train_data, method="rf",ntree = 600, metric="Precision", tuneGrid=tunegrid, trControl=control)
rf_gridsearch <- train(as.factor(outcome2)~., data=train_data, method="rf",ntree = 600, metric="Precision", tuneGrid=tunegrid, trControl=control)

rf_gridsearch <- train(as.factor(outcome2)~., data=train_data, method="rf",ntree = 600, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)




plot.roc(test_data$outcome1_avg, test.rf$votes[,1], percent=TRUE, col="#4daf4a", lwd=4,
         print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logistic Regression", "Random Forest"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)


##https://www.edureka.co/blog/random-forest-classifier/

##Choropleth maps
library("choroplethr")
library("choroplethrMaps")


map1 <- mydata
map1 <- filter(map1, Year == 2017)
map1 <- filter(map1, State =="VA")
map1$prediction <- predict(test.rf, map1, type = "response")
map1$correct <- ifelse(map1$outcome2 == map1$prediction,3, 176)

bbb <- select(map1, FIPS.County.Code, correct)
bbb$correct <- as.numeric(bbb$correct)
range <- c("virginia")
colnames(bbb) <- c("region", "value") 

county_choropleth(bbb, title = "Accuracy of prediction for models",legend = "Correct guessses ", 
    num_colors = 0, state_zoom = (range))


choro = CountyChoropleth$new(bbb)
choro$title = "Prescription Rates by County"
choro$add_state_outline
choro$set_zoom(c(range))
choro$legend = "prescription rates per 1000 people"


choro$render()


##http://www.storybench.org/build-interactive-county-level-map-tableau/
##https://www.clearlyandsimply.com/clearly_and_simply/2009/10/choropleth-maps-with-tableau.html
  
