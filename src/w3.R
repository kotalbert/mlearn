# Week 3, quiz

# 1.
# Load the cell segmentation data from the AppliedPredictiveModeling package using the commands: 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
training <- subset(segmentationOriginal, Case == "Train")
testing <- subset(segmentationOriginal, Case == "Test")
training$Case <- NULL
testing$Case <- NULL

# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
set.seed(125)
cartfit <- train(Class ~ ., method="rpart", data=training)
print(cartfit)

plot(cartfit$finalModel, uniform=TRUE)
text(cartfit$finalModel, use.n=T, all=T, cex=.8)
dev.off()

# 3. In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
newdat <- data.frame(TotalIntench2=c(23e3,5e3,57e3,NA), 
                     FiberWidthCh1=c(10, 10, 8, NA),
                     PerimStatusCh1=c(1,

a=data.frame(TotalIntench2 = 23e3, FiberWidthCh1 = 10, PerimStatusCh1=2)
predict(cartfit, a)

# 3. 

# Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree command with all defaults

library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

olivefit <- train(Area~., method="rpart", data=olive)
predict(olivefit, newdata)

# 4.
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and fit a logistic regression model 
# (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome 
# and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, 
# and low density lipoprotein cholesterol as predictors. 
# Calculate the misclassification rate for your model using this function and a prediction on the 
# "response" scale:

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
glmfit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, 
                method="glm", family=binomial, data=trainSA)

trainpreds <- data.frame(val=trainSA$chd, pred=predict(glmfit, trainSA))
testpreds <- data.frame(val=testSA$chd, pred=predict(glmfit, testSA))

trainmiss <- missClass(trainpreds$val, trainpreds$pred)
testmiss <- missClass(testpreds$val, testpreds$pred)
testmiss
trainmiss

# 5.
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. 
# Then set the seed to 33833. Fit a random forest predictor relating the factor
# variable y to the remaining variables. Read about variable importance in randomi
# forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by default the Gini importance. 
# Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?
# [NOTE: Use randomForest() specifically, not caret, as there's been some issues reported 
# with that approach. 11/6/2016]

vowel.train$y  <- factor(vowel.train$y)
vowel.test$y  <- factor(vowel.test$y)

set.seed(33833)
rfit <- randomForest(y~.,data=vowel.train, method="rf", importance=FALSE)
rfit
varImp(rfit)

