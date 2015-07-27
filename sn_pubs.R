## Required Libraries
library(caret)
library(flexclust)
library(randomForest)
library(miscTools)


mashpubs=read.csv(file.choose(),header = TRUE)
pubs = mashpubs

pubs$url=NULL
pubs$timedelta=NULL
pubs$data_channel_is_bus=NULL
pubs$data_channel_is_tech=NULL
pubs$data_channel_is_world=NULL
pubs$data_channel_is_socmed=NULL
pubs$data_channel_is_lifestyle=NULL
pubs$data_channel_is_entertainment=NULL
pubs$shares=NULL

preproc = preProcess(pubs)
normdata = predict(preproc,pubs)

km = kmeans(normdata,centers = 3)
km.kcca = as.kcca(km,normdata)
clusterdata = predict(km.kcca)

table(clusterdata)

cluster1 = subset(mashpubs,clusterdata==1)
cluster2 = subset(mashpubs,clusterdata==2)
cluster3 = subset(mashpubs,clusterdata==3)

modelRef1 = randomForest(log(shares) ~ ., data = cluster1, ntree=10000, importance=TRUE, keep.forest=TRUE, do.trace=1000)
modelRef2 = randomForest(log(shares) ~ ., data = cluster2, ntree=10000, importance=TRUE, keep.forest=TRUE, do.trace=1000)
modelRef3 = randomForest(log(shares) ~ ., data = cluster3, ntree=10000, importance=TRUE, keep.forest=TRUE, do.trace=1000)





