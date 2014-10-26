train<-read.csv("train.csv")
# take a random sample: 70% of train dataset 
set.seed(1)
mytrain.idx <- sample(1:nrow(train), nrow(train)*0.7,replace=FALSE)
mytrain <- train[mytrain.idx,]
mytest <- train[-mytrain.idx,]
write.csv(mytrain,file="mytrain.csv")
write.csv(mytest,file="mytest.csv")
