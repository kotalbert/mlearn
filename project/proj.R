# Practical mashine learning
# Course project program

# Load the libraries
library(dplyr)
library(caret)

set.seed(12345)

# Read the data
# Substitute errors as NAs
traind <- read.csv('data/pml-training.csv', na.strings=c("NA","#DIV/0!",""))
testd <-  read.csv('data/pml-testing.csv', na.strings=c("NA","#DIV/0!",""))

# function to remove metadata from data set 
mdr <- function(s) {

    o <- s %>% dplyr::select(-c(X, user_name, raw_timestamp_part_1, 
                raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))
    return(o)

}

# get index of target variable in dataset
ti <- function(s) {
    i <- which(colnames(traind) == "classe")
    return(i)
}

# remove metadata from data sets
traind <- mdr(traind)
testd <- mdr(testd)

# divide training dataset to training and validation
it <- caret::createDataPartition(traind$classe, p=0.5, list=FALSE)
trn <- traind[it, ]
val <- traind[-it, ]

# find index of target variable
i <- ti(trn)

# Remove near zero variables form the data sets
nzvar <- caret::nearZeroVar(trn[-i])

nzvt <- trn[-nzvar]
nzvv <- val[-nzvar]
nzvnd <- testd[-nzvar]

# Impute missing data with knnImpute method, on training, validation and new data
inpobj <- preProcess(nzvt[-i], method="knnImpute")

inptrain <- predict(inpobj, nzvt[-i])
inpval <- predict(inpobj, nzvv[-i])
inptest <- predict(inpobj, nzvnd)


# Fit tree
rpfit <- train(classe~., data=inptrain, method="rpart")
rpval <- predict(rpfit, inpval)


# Reduce predictors with PCA
pcaobj <- preProcess(inptrain[-i], method="pca")
pcatrain <- predict(pcaobj, inptrain[-i])
pcatval <- predict(pcaobj, inpval[-i])

# Fit boosting and rf
bfit <- train(classe~.,data=pcatrain, method="gbm", verbose=FALSE)
rffit <- train(classe~.,data=pcatrain, method="rf")

# Save for reuse
save(list=c("bfit", "rffit", "rpfit"), file="models")

# Validate
load("models2")

pcaval <- predict(pcaobj, inpval)
bval <- predict(bfit, pcaval)
rfval <- predict(rffit, pcaval)

# Calculate confusion on validation data

confusionMatrix(rpval, inpval$classe)
confusionMatrix(bval, inpval$classe)
confusionMatrix(rfval, inpval$classe)

# Predict on new data
pcatest <- predict(pcaobj, inptest)
predtest <- predict(rffit, pcatest)
data.frame(inptest$problem_id, predtest)

