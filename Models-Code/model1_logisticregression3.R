library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(caret)
library(pglm)


setwd("R files/practice files/accenture/datasets/model/County")
mydata <- read.csv("combined_prescription.csv", stringsAsFactors = FALSE)

colnames(mydata) <- c("x","State", "Year", "policy1","policy2","policy3","policy4","policy5","policy6", "policy7",
                      "policy8", "policy9", "policy10", "policy11", "policy12", "policy13", "policy14", "policy15", "policy16", "policy17", "policy18",
                      "policy19", "key2", "Population", "percent_male", "percent_female", "under_18", "X18.24", "X25.44", "X45.64", "over_65", "Mean_income", 
                      "Unemployment_rate", "County", "FIPS.County.Code", "Prescribing.Rate", "outcome1", "outcome2", "outcome3")

mydata <- mydata[,-c(1,23,34,35, 36,37,39)]
mydata[,22:31] <- scale(mydata[,22:31])
##d_panel <- pdata.frame(mydata, index=c("State"))

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

## Logistic regression model

glm.fit <- glm(outcome2 ~ Year + (factor(State)-1) + policy1 + policy2 + policy3 + policy4 + policy5 + policy6 + policy7 + policy8
             + policy9 + policy10 + policy11 + policy12 + policy13 + policy14 + policy15 + policy16 + policy17 
             + policy18 + Population + percent_male + under_18 + X18.24 + X25.44 + X45.64 + over_65 
             + Mean_income + Unemployment_rate + policy16*policy18 + policy16*Unemployment_rate, data=train_data, family = binomial)



summary(glm.fit)

# calculate McFadden's pseudo R^2. 
# we can pull the log-likelihood of the null model out of the logistic variable by 
# getting the value for the null deviance and dividing by-2
ll.null <- glm.fit$null.deviance/(-2)
ll.proposed <- glm.fit$deviance/(-2)
(ll.null-ll.proposed)/ll.null
# pseudo R^2 is 0.157964. This can be interpreted as the overall effect size
# use chi-square distribution to calculate a P-value for R^2
1 - pchisq(2*(ll.proposed-ll.null), df=(length(glm.fit$coefficients)-1))
# in this case, the p-value is so tiny that the outcome close to 0
# check the length of two columns

##combining predictions with testing set
test_data$prob <- predict(glm.fit, test_data, type = "response")
test_data$prediction <- ifelse(test_data$prob > .45, 1, 0)
##this is confusion matrix
con_mat <- table(test_data$prediction, test_data$outcome2)

##Generate S curve
test3 <- arrange(test_data, prob)
index <- c(1:nrow(test3))
test3 <- cbind(test3, index)
ggplot(data=test3, aes(x=index, y=prob)) +
  geom_point(aes(color=test_data$outcome2), alpha=1, shape=1, stroke=1) +
  xlab("Index") +
  ylab("Probability of outcome")


##Analysis of results
roc(test_data$outcome2, test_data$prob, plot = TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",col="#377eb8", lwd=4,
    print.auc=TRUE)


ROC1 <- roc(test_data$outcome2, test_data$prob)
AUC1 <- as.numeric(auc(ROC1))
accuracy <- (con_mat[1,1] + con_mat[2,2])/ (nrow(test_data))
precision <- con_mat[1,1]/ (con_mat[1,1] + con_mat[1,2])
recall<- con_mat[1,1]/ (con_mat[1,1] + con_mat[2,1])
results <- cbind(AUC1, accuracy, precision, recall)

control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
rf_gridsearch <- train(as.factor(outcome2)~., data=train_data, method="glm", metric="Accuracy",trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

plot.roc(test_data2$outcome2, test_data2$prob, percent=TRUE, col="#4daf4a", lwd=4,
print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logistic Regression", "Random Forest"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)


