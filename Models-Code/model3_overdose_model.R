# read files
overdose <- read.csv("overdose.csv")
nrow(overdose)
head(overdose)
summary(overdose)
colnames(overdose)[10] = "Male"
colnames(overdose)[11] = "Female"
colnames(overdose)[12] = "under18"
colnames(overdose)[16] = "over65"
colnames(overdose)[17] = "Income"
colnames(overdose)[18] = "Unrate"
colnames(overdose)[21] = "Deathrate"

# add Deaths and Alives column
Deaths=round(overdose$Population*overdose$Deathrate/100, 0)
Alives=overdose$Population-Deaths
mydata=data.frame(overdose,Deaths,Alives)
head(mydata)
nrow(mydata)

# for multiple linear regression
#1 response variable is Death Counts
#
fit1 <- lm(Deaths ~ policy1 + policy2 + policy3 + policy4 + policy5, data=mydata)
summary(fit1)
line_model1 <- step(fit1)
summary(line_model1)

fit2 <- lm(Deaths ~ (policy1 + policy2 + policy3 + policy4 + policy5)^2, data=mydata)
summary(fit2)
line_model2 <- step(fit2)
summary(line_model2)

#2
# predictors are demographic variables
fit3 <- lm(Deaths ~ Population + Male + under18 + over65 + Income + Unrate, data=mydata)
summary(fit3)
line_model3 <- step(fit3)
summary(line_model3)

#3
# predictors is the combination of policies and demographic variables
fit4 <- lm(Deaths ~ policy1 + policy2 + policy3 + policy4 + policy5 + Population + Male + X18.24 + Income + Unrate, data=mydata)
summary(fit4)
line_model4 <- step(fit4)
summary(line_model4)

fit5 <- lm(Deaths ~ (policy1 + policy2 + policy3 + policy4 + policy5 + Population + Male + X18.24 + Income + Unrate)^2, data=mydata)
summary(fit5)
line_model5 <- step(fit5)
summary(line_model5)


# for logistic regression
#1
# response variable is outcome

mylogit1<-glm(outcome ~ policy1 + policy2 + policy3 + policy4 + policy5,
              data=mydata, family = binomial)
summary(mylogit1)
model1 <- step(mylogit1)
summary(model1)
# pearson's chi-square test
1-pchisq(1177.3,958)

#2
# predictors are combination of policies and demographic variables
mylogit2<-glm(outcome ~ policy1 + policy2 + policy3 + policy4 + policy5 + Male + under18 + over65 + Income + Unrate,
              data=mydata, family = binomial)
summary(mylogit2)
model2 <- step(mylogit2)
summary(model2)

#3
# contingency table of Deaths and Alives
myvar1 <-c(3:7,22:24)
mytable1 <- mydata[myvar1]
head(mytable1)
ftable(xtabs(cbind(Deaths,Alives)~ policy1 + policy2 + policy3 + policy4 + policy5,
                          data=mydata))
  contable1 <- read.csv("policy.csv")
mylogit3<-glm(cbind(Deaths,Alives) ~ policy1 + policy2 + policy3 + policy4 + policy5,
              data=contable1, family = binomial)
summary(mylogit3)
model3 <- step(mylogit3)
summary(model3)
# pearson's chi-square test
1-pchisq(27892,4)

#4
# predictors are demographic variables
mylogit4<-glm(cbind(Deaths,Alives) ~ Male + under18 + X18.24 + X25.44 + X45.64 + over65 + Income + Unrate,
              data=mydata, family = binomial)
summary(mylogit4)
model4<-step(mylogit4)
summary(model4)
# Hosmer-Lemeshow test
hoslem.test(mydata$Deaths,fitted(model4))

#5
# predictors are combination of policies and demographic variables
mylogit5<-glm(cbind(Deaths,Alives) ~ policy1 + policy2 + policy3 + policy4 + policy5 + Male + under18 + over65 + Income + Unrate,
              data=mydata, family = binomial)
summary(mylogit5)
model5<-step(mylogit5)
summary(model5)

# Hosmer-Lemeshow test
library(ResourceSelection)
hoslem.test(mydata$Deaths,fitted(model5))
hoslem.test(mydata$Alives,fitted(model5))


# split into training and validation
row<-nrow(mydata)
row
trainindex <- sample(row, 0.6*row, replace=FALSE)
training <- mydata[trainindex,] # Training dataset
nrow(training)
head(training)
validation <- mydata[-trainindex,]
nrow(validation)

#6
# predictors are demographic variables
mylogit6<-glm(cbind(Deaths,Alives) ~ Male + under18 + X18.24 + X25.44 + X45.64 + over65 + Income + Unrate,
              data=training, family = binomial)
summary(mylogit6)
model6<-step(mylogit6)
summary(model6)
# Hosmer-Lemeshow test
hoslem.test(training$Deaths,fitted(model6))
hoslem.test(training$Alives,fitted(model6))
