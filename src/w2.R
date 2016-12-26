# Week 2, quiz

# 1
#  create non-overlapping training and test sets with about 50% of the observations assigned to each
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

# 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Plot the outrome versus index of samples
training$id <- seq(1:nrow(training))

p <- ggplot(training, aes(x=id, CompressiveStrength))
p+geom_point(aes(color=cut(Water,10)))
p+geom_point(aes(color=cut(FineAggregate,10)))
p+geom_point(aes(color=cut(BlastFurnaceSlag,10)))
p+geom_point(aes(color=cut(Superplasticizer,10)))
p+geom_point(aes(color=cut(Age,10)))
p+geom_point(aes(color=cut(FlyAsh,10)))
p+geom_point(aes(color=cut(CoarseAggregate,10)))
dev.off()


# 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)

inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

training$supplaslog <- log10(training$Superplasticizer+1)

# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?

ggplot(training, aes(Superplasticizer)) + geom_histogram()
ggplot(training, aes(supplaslog)) + geom_histogram()
dev.off()

# 4
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433); data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. How many are there?

ilvars <- dplyr::select(training, starts_with("IL"))

pca <- preProcess(ilvars, method="pca", thresh=0.8)
print(pca)

# 5
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)

set.seed(3433)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
# Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. 
# Use method="glm" in the train function. 
# What is the accuracy of each method in the test set? Which is more accurate?
 
# Select variable names as required
iltrain <- dplyr::select(training, starts_with("IL"))
iltest <- dplyr::select(testing, starts_with("IL"))

# Calulationg PCA on training variables
pca <- preProcess(ilvars, method="pca", thresh=0.8)

# Convert data set as PCA matrix
train_pca <- predict(pca, iltrain)
test_pca  <- predict(pca, iltest) 

# Combine with predicted variable
train_pca$diagnosis  <- training$diagnosis
test_pca$diagnosis  <- testing$diagnosis

# Training model on PCA variables
trainfitpca <- train(diagnosis~., method="glm", data=train_pca)
testfitpca <- train(diagnosis~., method="glm", data=test_pca)

# Training model on non converted variables
iltrain$diagnosis <- training$diagnosis
iltest$diagnosis <- testing$diagnosis

trainfit <- train(diagnosis~., method="glm", data=iltrain)
testfit <- train(diagnosis~., method="glm", data=iltest)

# Model comparision with comfusionMatrix
confusionMatrix(testfitpca)
confusionMatrix(testfit)


