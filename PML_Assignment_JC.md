---
title: "PML Assignment"
author: "JC"
output:
  html_document:
    keep_md: yes
---

## Report Objective

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. The objective of this report is the prediction of the manner in which users of exercise device exerecise.

## Data

The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

Empty the workspace, load all libraries used, and set seed for reproducibility.


```r
rm(list=ls(all=TRUE))

library(ElemStatLearn)
library(caret)
library(rpart)
library(randomForest)
library(RCurl)
set.seed(1996)
```

### Download and Read Training Dataset




```r
trainingLink <- getURL("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
pml_CSV  <- read.csv(text = trainingLink, header=TRUE, sep=",", na.strings=c("NA",""))

pml_CSV <- pml_CSV[,-1] # Remove the first column that represents a ID Row
```

### Partition data into Training and Validation


```r
inTrain = createDataPartition(pml_CSV$classe, p=0.60, list=FALSE)
training = pml_CSV[inTrain,]
validating = pml_CSV[-inTrain,]

# number of rows and columns of data in the training set
dim(training)
```

```
## [1] 11776   159
```

```r
# number of rows and columns of data in the validating set
dim(validating)
```

```
## [1] 7846  159
```

## Data Exploration and Cleaning

We have too many columns in our dataset. We want to reduce the sparsity of the dataset.
So, we remove columns that have less than 60% of data entered.


```r
# Number of cols with less than 60% of data
sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))
```

[1] 100

```r
# apply our definition of remove columns that most doesn't have data, before its apply to the model.

Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training   <-  training[,Keep]
validating <- validating[,Keep]

# number of rows and columns of data in the final training set

dim(training)
```

[1] 11776    59

```r
# number of rows and columns of data in the final validating set

dim(validating)
```

[1] 7846   59


## Modeling
In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. It is estimated internally, during the execution. So, we proceed with the training the model (Random Forest) with the training data set.


```r
model <- randomForest(classe~.,data=training)
print(model)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.13%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3347    1    0    0    0 0.0002986858
## B    3 2276    0    0    0 0.0013163668
## C    0    4 2048    2    0 0.0029211295
## D    0    0    2 1927    1 0.0015544041
## E    0    0    0    2 2163 0.0009237875
```

### Model Evaluation

#### Confusion Matrix


```r
confusionMatrix(predict(model,newdata=validating[,-ncol(validating)]),validating$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232    2    0    0    0
##          B    0 1516    0    0    0
##          C    0    0 1364   12    0
##          D    0    0    4 1274    3
##          E    0    0    0    0 1439
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9973          
##                  95% CI : (0.9959, 0.9983)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9966          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9987   0.9971   0.9907   0.9979
## Specificity            0.9996   1.0000   0.9981   0.9989   1.0000
## Pos Pred Value         0.9991   1.0000   0.9913   0.9945   1.0000
## Neg Pred Value         1.0000   0.9997   0.9994   0.9982   0.9995
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1932   0.1738   0.1624   0.1834
## Detection Prevalence   0.2847   0.1932   0.1754   0.1633   0.1834
## Balanced Accuracy      0.9998   0.9993   0.9976   0.9948   0.9990
```

#### Accuracy with validation data


```r
accuracy <-c(as.numeric(predict(model,newdata=validating[,-ncol(validating)])==validating$classe))

accuracy <-sum(accuracy)*100/nrow(validating)
```

Model Accuracy as tested over Validation set = **99.7%**.  

## Prediction
We apply the same data cleaning operations on it and coerce all columns of testing data set for the same class of previous data set. 

### Getting Testing Dataset and Preparing it


```r
testingLink <- getURL("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
pml_CSV  <- read.csv(text = testingLink, header=TRUE, sep=",", na.strings=c("NA",""))

pml_CSV <- pml_CSV[,-1] # Remove the first column that represents a ID Row
pml_CSV <- pml_CSV[ , Keep] # Keep the same columns of testing dataset
pml_CSV <- pml_CSV[,-ncol(pml_CSV)] # Remove the problem ID

# Apply the Same Transformations and Coerce Testing Dataset

# Coerce testing dataset to same class and strucuture of training dataset 
testing <- rbind(training[100, -59] , pml_CSV) 

# Apply the ID Row to row.names
row.names(testing) <- c(100, 1:20)
```

#### Predicting with testing dataset


```r
predictions <- predict(model,newdata=testing[-1,])
print(predictions)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
