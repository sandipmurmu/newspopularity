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

