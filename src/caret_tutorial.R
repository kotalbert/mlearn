# Caret package tutorial
# http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf

library(caret)
data(segmentationData)

# Data segmentationi, base functions
segmentationData$Cell <- NULL
training <- subset(segmentationData, Case == "Train")
testing <- subset(segmentationData, Case == "Test")
training$Case  <- NULL
testing$Case <- NULL

# Data partitioning
datapart <- createDataPartition(y=segmentationData$Class, list=F)
trn <- segmentationData[datapart,]
tst <- segmentationData[-datapart,]
