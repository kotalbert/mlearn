# Practical Machine Learning Course Project
Pawel Daniluk  
2017-01-20  



# Summary 

This report is a brief demonstration of machine learning techniques, discussed in "Practical Machine Learning" course by Johns Hopkins University. The data set provided for the exercise was pre processed, reduced in dimensions and used to train three prediction models.

Since the goal of this project is to demonstrate basic machine learning functionalities, the statistical analysis of the problem was not conducted. The feature

For the purpose of this project, `caret` package was used. Three basic model models were fitted:

-   Classification Tree
-   Boosting
-   Random Forest

Random Forrest model produced most accurate predictions, achieving 92% on validation subset, with dimensions reduced with PCA algorithm. The prediction accuracy on test data (verified by quiz) was 95%.


# Training Data set

The training data set is [Human Activity Recognition](http://groupware.les.inf.puc-rio.br/har#weight_lifting_exercises) data, provided for open use by the authors of original study[^1].

The data describes simple physical activity, measured with set of sensors. The goal of the study was to correctly identify, weather the exercise was performed correctly. The level of exercise "correctness" is measured on A do E scale. The data set consists of 160 predictors and 19622 samples. Target variable is named `classe`. 

[^1]: Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. 

# Study design 

In order to train and validate a machine learning model, the training set must be divided into subsets. The common strategy is to divide the available data into training and test subsets. The training data was used to select predictors and train models, while the validation subset was only used for measuring models accuracy.

First, the data is loaded into memory, and metadata, like user name or time stamp are dropped.


```r
# Helper functions for data preprocessing
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

# Read the data from text file, correctly recognize missing data
traind <- read.csv('data/pml-training.csv', na.strings=c("NA","#DIV/0!",""))

# remove metadata from data sets
traind <- mdr(traind)
```

Since the data set was relatively large, it was safe to divide the data into two equally large groups. Random seed is fixed to be 12345 numeral, for reproducibility.


```r
# Fix random seed for reproducibility
set.seed(12345)
# Partition the data
it <- caret::createDataPartition(traind$classe, p=0.5, list=FALSE)
trn <- traind[it, ]
val <- traind[-it, ]
```

# Pre processing and dimension reduction

Since this analysis was performed on a single computer, not on large computational cluster, even relatively small dataset required large amounts of processing power. Therefore some reduction of predictor number was required. The near zero variance predictors were removed and then number of predictors were further reduced with Principal Components Analysis.


```r
# find index of target variable in set 
i <- ti(trn)
# Remove near zero variables form the data sets
nzvar <- caret::nearZeroVar(trn[-i])

nzvt <- trn[-nzvar]
nzvv <- val[-nzvar]
```
It was observed, that some predictors have missing values. For most machine learning techniques, missing data must be dealt with. One of simple techniques discussed is k-nearest neighbours method.


```r
# Impute missing data with knnImpute method, on training, validation and new data
inpobj <- preProcess(nzvt[-i], method="knnImpute")

# Apply to all data sets
inptrain <- predict(inpobj, nzvt[-i])
inpval <- predict(inpobj, nzvv[-i])
```

Finally, after imputing missing data, the data set dimensions can be further reduced with PCA.


```r
# Reduce predictors with PCA
pcaobj <- preProcess(inptrain[-i], method="pca")
pcatrain <- predict(pcaobj, inptrain[-i])
pcaval <- predict(pcaobj, inpval[-i])
```

# Model training

Three basic models were fit. The classification tree, using `rpart` method, boosting, using `bgm` and random forest, using `rf` method. Every method requires, that there is no missing data in training set. Additionally, random forest and boosting are computationally intensive, therefore both models were trained on data reduced by PCA.


```r
# Train classification tree on not reduced data
rpfit <- train(classe~., data=inptrain, method="rpart")

# Train  models on reduced data
bfit <- train(classe~.,data=pcatrain, method="gbm", verbose=FALSE)
rffit <- train(classe~.,data=pcatrain, method="rf")
```

# Models validation

## Definition of error measure

In order to validate the prediction model, a definition of prediction error measure must be developed. In this problem, the most suitable measure is Accuracy, defined as percentage of True Positives in total predictions.

The `caret` package has a `confusionMatrix` function, which delivers Accuracy, as well as other statistics, such as Sensitivity and Specificity. This function deliverers also a confusion matrix, displaying predicted and actual values, which can be used to assess areas of poor predictions.

## Prediction 

After models were trained, their accuracy on validation subset can be compared. First, models must be applied to new data, using `predict` method, and then accuracy can be measured using `confusionMatrix`. 


```r
# Apply models to validation sample
rpval <- predict(rpfit, inpval)
bval <- predict(bfit, pcaval)
rfval <- predict(rffit, pcaval)
```

It can be observed, that both boosting and random forest methods are significantly more accurate than classification tree, despite being trained on reduced data set. 

## Decision tree confusion matrix


```r
confusionMatrix(rpval, inpval$classe)
```
```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1687  282   47   82   30
         B  403 1215  146  510  439
         C  670  377 1486  587  455
         D   21   24   32  429   50
         E    9    0    0    0  829

Overall Statistics
                                          
               Accuracy : 0.5755          
                 95% CI : (0.5657, 0.5853)
    No Information Rate : 0.2844          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.4669          
 Mcnemar's Test P-Value : < 2.2e-16
```

## Boosting confusion matrix


```r
confusionMatrix(bval, inpval$classe)
```

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2395  221  189   87   64
         B   94 1336  140   59  175
         C  142  180 1227  210  116
         D  113   73   73 1211  115
         E   46   88   82   41 1333

Overall Statistics
                                          
               Accuracy : 0.7647          
                 95% CI : (0.7562, 0.7731)
    No Information Rate : 0.2844          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.7019          
 Mcnemar's Test P-Value : < 2.2e-16
```

## Random Forest confusion matrix


```r
confusionMatrix(rfval, inpval$classe)
```

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2651   65   38   18    5
         B   52 1732   59   12   24
         C   49   66 1533  119   18
         D   23    8   36 1434   32
         E   15   27   45   25 1724

Overall Statistics
                                          
               Accuracy : 0.925           
                 95% CI : (0.9196, 0.9301)
    No Information Rate : 0.2844          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9051          
 Mcnemar's Test P-Value : 1.923e-10
```
