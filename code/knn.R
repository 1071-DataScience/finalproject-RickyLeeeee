library(class)
library(dplyr)

n <- nrow(AQI)
trainLabels <- traindata$AQI
knnTrain <- subset(traindata, select = -c(AQI))
knnTest <- subset(testdata, select = -c(AQI))

kv <- round(sqrt(n))
kv

prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = kv)

knncm <- table(x = testdata$AQI, y = prediction, dnn = c("實際", "預測"))
knncm

knnaccuracy <- sum(diag(knncm)) / sum(knncm)
knnaccuracy

sumary(prediction)
