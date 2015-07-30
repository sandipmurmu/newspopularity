##install.packages("caret")
##install.packages("flexclust")
##install.packages("randomForest")
##install.packages("miscTools")


## Required Libraries
library(caret)
library(flexclust)
library(randomForest)
library(miscTools)

ds = read.csv(file.choose(), header=TRUE)

##Data partition into 2 subsets - 80% train and 20% test
indexes = sample(1:nrow(ds), size = 0.2*nrow(ds))
test = ds[indexes,]
dim(test)
train = ds[-indexes,]
dim(train)


##Create Clusters using K-means
strain=train

strain$url=NULL
strain$timedelta=NULL
strain$data_channel_is_bus=NULL
strain$data_channel_is_tech=NULL
strain$data_channel_is_world=NULL
strain$data_channel_is_socmed=NULL
strain$data_channel_is_lifestyle=NULL
strain$data_channel_is_entertainment=NULL

strain$is_weekend=NULL
strain$weekday_is_monday=NULL
strain$weekday_is_tuesday=NULL
strain$weekday_is_wednesday=NULL
strain$weekday_is_thursday=NULL
strain$weekday_is_friday=NULL
strain$weekday_is_saturday=NULL
strain$weekday_is_sunday=NULL
  

stest = test

stest$url=NULL
stest$timedelta=NULL
stest$data_channel_is_bus=NULL
stest$data_channel_is_tech=NULL
stest$data_channel_is_world=NULL
stest$data_channel_is_socmed=NULL
stest$data_channel_is_lifestyle=NULL
stest$data_channel_is_entertainment=NULL

stest$is_weekend=NULL
stest$weekday_is_monday=NULL
stest$weekday_is_tuesday=NULL
stest$weekday_is_wednesday=NULL
stest$weekday_is_thursday=NULL
stest$weekday_is_friday=NULL
stest$weekday_is_saturday=NULL
stest$weekday_is_sunday=NULL

## use caret functions to preprocess the data
prepProcess = preProcess(strain)
ntrain = predict(prepProcess,strain)
ntest = predict(prepProcess,stest)

##Create 3 clusers
km = kmeans(ntrain,centers = 3)

##Create classification for train and test
km.kcca = as.kcca(km,ntrain)
ctrain = predict(km.kcca)
ctest = predict(km.kcca, newdata=ntest)

table(ctrain)
table(ctest)

## Subset the train and test sets
train1 = subset(train, ctrain == 1)
train2 = subset(train, ctrain == 2)
train3 = subset(train, ctrain == 3)

test1 = subset(test, ctest == 1)
test2 = subset(test, ctest == 2)
test3 = subset(test, ctest == 3)

modelRef1 = randomForest(log(shares) ~ . - url, data = train1, ntree=1000, importance=TRUE, keep.forest=TRUE, do.trace=1000)
modelRef2 = randomForest(log(shares) ~ . - url, data = train2, ntree=1000, importance=TRUE, keep.forest=TRUE, do.trace=1000)
modelRef3 = randomForest(log(shares) ~ . - url, data = train3, ntree=1000, importance=TRUE, keep.forest=TRUE, do.trace=1000)

## Test the prediction on train set
pred_train1 = predict(modelRef1, train1)
pred_train2 = predict(modelRef2, train2)
pred_train3 = predict(modelRef3, train3)

preds_train = c(exp(pred_train1), exp(pred_train2), exp(pred_train3))
vals_train = c(train1$shares, train2$shares, train3$shares)

## Generate rsquared and rmse
rSquared(vals_train,vals_train - preds_train)
sqrt(mean((vals_train-preds_train)^2))


## Generate predictions on test set
pred_test1 = predict(modelRef1, test1)
pred_test2 = predict(modelRef2, test2)
pred_test3 = predict(modelRef3, test3)


## Combine results from individual sets after exponentiating the predictions
pred_test = c(exp(pred_test1), exp(pred_test2), exp(pred_test3))
allurls = c(test1$url, test2$url, test3$url)

## Sort as per id and generate the file for submission
out_df = data.frame(allurls, pred_test)
out_df = out_df[with(out_df, order(allurls, pred_test)), ]
names(out_df) = c("url", "predictions")
write.csv(out_df, "C:\babi\SandipMurmu.csv", row.names=F, quote=F)





