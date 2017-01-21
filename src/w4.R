# Week 4, quiz

# 1.
# Set the variable y to be a factor variable in both the training and test set.
# Then set the seed to 33833. 
# Fit (1) a random forest predictor relating the factor variable y to the 
# remaining variables and (2) a boosted 
# predictor using the "gbm" method. Fit these both with the train() command in 
# the caret package. 

# What are the accuracies for the two approaches on the test data set? 
# What is the accuracy among the test set samples where the two methods agree? 

library(ElemStatLearn)
library(dplyr)
library(caret)

data(vowel.train)
data(vowel.test)

v.train <- dplyr::mutate(vowel.train, y=as.factor(y))
v.test <- dplyr::mutate(vowel.test, y=as.factor(y))

set.seed(33833)

fitrf<- train(y~.,data=v.train, method="rf")
fitgbm <- train(y~.,data=v.train, method="gbm")

predrf  <- predict(fitrf, v.test)
predgbm <- predict(fitgbm, v.test) 

preds <- data.frame(prf = predrf, pgb = predgbm, y=v.test$y)
preds.agr <- dplyr::filter(preds, prf == pgb)

confusionMatrix(predrf, v.test$y)$overall[1]
confusionMatrix(predgbm, v.test$y)$overall[1]
confusionMatrix(preds.agr$prf, preds.agr$y)$overall[1]

# 2.
# Set the seed to 62433 and predict diagnosis with all the other variables using 
# a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis 
# ("lda") model. Stack the predictions together using random forests ("rf").
# What is the resulting accuracy on the test set? Is it better or worse than 
# each of the individual predictions? 

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# fitting model
fitrf <- train(diagnosis~., data=training, method="rf")
fitgbm <- train(diagnosis~., data=training, method="gbm")
fitlda <- train(diagnosis~., data=training, method="lda")

# predictions
prf <- predict(fitrf, testing)
pgbm <- predict(fitgbm, testing)
plda <- predict(fitlda, testing)

predDF <- data.frame(prf, pgbm, plda, diagnosis=testing$diagnosis)
fitcomb <- train(diagnosis~., data=predDF, method="rf")
pcomb <- predict(fitcomb, predDF)

dgn <- testing$diagnosis
# compare accuracy
confusionMatrix(prf,dgn)$overall[1]
confusionMatrix(pgbm,dgn)$overall[1]
confusionMatrix(plda,dgn)$overall[1]
confusionMatrix(pcomb,dgn)$overall[1]

# 3.

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
# Which variable is the last coefficient to be set to zero as the penalty 
# increases? (Hint: it may be useful to look up ?plot.enet). 

set.seed(233)
fitlasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)

# 4.
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)

fitts <- bats(tstrain)
fcast <- forecast(fitts, level=95, h=dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
 dim(testing)[1]

# 5.

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testin

set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
