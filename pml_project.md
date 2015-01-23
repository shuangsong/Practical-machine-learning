---
title: "Machine Learning project"
author: "Shuang"
output: html_document
---
Purpose and summary
----

This project is to quantify how well an individual perform for a particular activity.
In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. People were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). For its reproducible, I use set.seed, for prediction I use random forest to do it for the reason that although it is difficult to interpret but often very accurate.

packages I use:
-----

```r
rm(list=ls())
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(kernlab)
```

```
## Warning: package 'kernlab' was built under R version 3.1.2
```

```r
library(AppliedPredictiveModeling)
```

```
## Warning: package 'AppliedPredictiveModeling' was built under R version
## 3.1.2
```

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.2
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(ggplot2)
library(Metrics)
```

```
## Warning: package 'Metrics' was built under R version 3.1.2
```

Get and clean data:
----

```r
setwd("C:/Users/stephanie song/Desktop")
train_url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(train_url,destfile="~/training.csv")
download.file(test_url,destfile="~/testing.csv")
train<-read.csv("~/training.csv", na.strings=c("NA","#DIV/0!",""))
test<-read.csv("~/testing.csv",na.strings=c("NA","#DIV/0!",""))
```
Choose feature part to do the analysis. For the reason that from train data from 1 to 7 column is not necessary for prediction, then we subset train data: remove user name, window, and time. Remove the columns that contain NAs, because columns which contain NAs will not predict good. Use training set and split into training_train and training _test sets and build model on training set.The function createDataPartition can be used to create balanced splits of the data. If the y argument to this function is a factor, the random sampling occurs within each class and should preserve the overall class distribution of the data. For this project,  create a single 75% and 25% split of the data.



```r
training<-train[,-c(1:7)]
set.seed(100)
inTrain<-createDataPartition(training$classe, p=0.75, list=FALSE)
training_train<-training[inTrain,]
training_test<-training[-inTrain,]
dim(training_train)
training_train<-as.data.frame(training_train)
clean_train<-training_train[,colSums(is.na(training_train))==0]
clean_test<-training_test[,colSums(is.na(training_test))==0]
```

build model & analysis
-------



```r
set.seed(150)
tr<-trainControl(method="cv", number=5)
#first prediction model
modelFit1 <- train(classe ~.,data=clean_train, method="rf", trControl=tr)
prediction1<-predict(modelFit1, clean_train)
print(modelFit1)
summary(modelFit1$finalModel)
confusionMatrix(predict(modelFit1, newdata=clean_test), clean_test$classe)
table(prediction1, clean_train$classe)
```

Prediction
-----

```r
#predict train_test data
modelFit2<-train(classe~., data=clean_test, method="rf", trControl=tr)
prediction2<-predict(modelFit1, newdata=clean_test)
table(prediction2, clean_test$classe)
sum(clean_test$classe==predict(modelFit1, clean_test)) / nrow(clean_test)
test<-test[,colSums(is.na(test))==0]
test<-test[,-c(1:7)]
#predict test data
prediction3<-predict(modelFit2, newdata=test)
```


Submission
---

```r
#coursera code 
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(prediction3)
```


conclusion
----
Out of sample error is 0.993. Confusion matrix is relatively accurate.

references
-----

Split a Data Frame into Testing and Training Sets in R 

Appendix
----
Figure 1 plot of modelFit

```r
ggplot(modelFit1)
```

Figure 2 Importance of variables

```r
resize.win <- function(Width=6, Height=6)
{
        # windows
        dev.off(); 
        windows(record=TRUE, width=Width, height=Height)
}
resize.win(5,5)
plot(rnorm(100))
resize.win(10,10)
plot(rnorm(100))
plot(varImp(modelFit1))
```

Figure 3 Prediction plot


```r
qplot(predict(modelFit1,clean_test), classe, data=clean_test)
```








