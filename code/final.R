#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages('ROCR')
#install.packages('randomForest')

library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(class)
library(dplyr)


# Read the data and scale the data
data <- read.csv('./data/AllFeatures+Labelv4.csv', header = TRUE, sep = ',')
data[,5:ncol(data)] <- scale(data[,5:ncol(data)], center = TRUE, scale = TRUE)

# Remove unused columns
#AQI <- subset(data, 汽車臺數!=0, select = -c(統計年, 統計月, 縣市別, 罰營建工程次數, 罰其他固定污染源次數,
                                            #檢營建工程次數, 罰移動污染源次數, 人均日廢棄物KG, 檢其他固定污染源次數))
AQI <- subset(data, 汽車臺數!=0, select = c(AQI, 汽車臺數, 機車臺數))
trainLabels <- AQI$AQI

#AQI$AQI <- ifelse(AQI$AQI == 'green', 1, 0)

# Randomly shuffle the data
n <- nrow(AQI)
set.seed(1111)
newAQI <- AQI[sample(n),]

# Split into training and testing data
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- newAQI[t_idx,]
testdata <- newAQI[ - t_idx,]


p.green <- 0
p.yellow <- 0
p.orange <- 0
p.red <- 0
r.green <- 0
r.yellow <- 0
r.orange <- 0
r.red <- 0
f1.green <- 0
f1.yellow <- 0
f1.orage <- 0
f1.red <- 0
knn.accu <- 0
dt.accu <- 0
rf.accu <- 0

# Create n equally size folds
folds <- cut(seq(1, nrow(AQI)), breaks = n, labels = FALSE)
n <- 5
# ----- n fold ----- #
for(i in c(1:n)){
  cat('Now executing the fold', i, '...\n')
  #Segement data by fold using the which() function
  #if ((i-1) %% n == 0){
  #  j <- n
  #} else {
  #  j <- (i-1) %% n
  #}
  testIndexes <- which(folds==i,arr.ind=TRUE)
  #validIndexes <- which(folds==j,arr.ind=TRUE)
  
  # -----Split the data set ----- #
  testdata <- AQI[testIndexes, ]
  test.label <- trainLabels[testIndexes]

  #valid.Data <- AQI[validIndexes, ]
  #valid.label <- trainLabels[validIndexes]
  
  traindata <- AQI[-testIndexes, ]
  train.label <- trainLabels[-testIndexes]
  
  # ----- Decision Tree ----- #
  dTModel <- rpart(formula = AQI ~ ., data = traindata, 
                      method = "class", control = rpart.control(cp = 0.001))
  #fancyRpartPlot(dTModel)
  
  # Prediction
  dTresult <- predict(dTModel, newdata = testdata, type = "class")
  dTCM <- table(testdata$AQI, dTresult, dnn = c("實際", "預測"))
  dt.accu <- dt.accu + sum(diag(dTCM)) / sum(dTCM)
  
  # Plotting the decision tree
  #print(dTModel)
  #pdf("decisionTree.pdf")
  #par(cex=0.7)
  #plot(dTModel)
  #text(dTModel)
  #dev.off()
  
  
  # ----- Random Forest ----- #
  rFModel <- randomForest(AQI ~ ., data = traindata, importane = T, 
                               proximity = T, do.trace = 100)
  #rFModel
  #plot(rFModel)
  
  #Prediction
  rFresult <- predict(rFModel, newdata = testdata, type = "class")
  rFCM <- table(testdata$AQI, rFresult, dnn = c("實際", "預測"))
  rf.accu <- rf.accu + sum(diag(rFCM)) / sum(rFCM)
  
  
  # ----- KNN ----- #
  
  knnTrain <- subset(traindata, select = -c(AQI))
  knnTest <- subset(testdata, select = -c(AQI))
  
  kv <- round(sqrt(n))
  #kv
  
  knnResult <- knn(train = knnTrain, test = knnTest, cl = train.label, k = 6)
  knnCM <- table(x = test.label, y = knnResult, dnn = c("實際", "預測"))
  knnaccuracy <- sum(diag(knnCM)) / sum(knnCM)
}


# choose best k
klist <- seq(1:50)
knnFunction <- function(x, knnTrain, knnTest, trainLabels, testLabels) {
  prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = x)
  cm <- table(x = testLabels, y = prediction)
  accuracy <- sum(diag(cm)) / sum(cm)
}
accuracies <- sapply(klist, knnFunction, knnTrain = knnTrain, knnTest = knnTest, trainLabels = trainLabels, testLabels = testdata$AQI)

# k value visualization
df <- data.frame(
  kv = klist, accuracy = accuracies)

ggplot(df, aes(x = kv, y = accuracy, label = kv, color = accuracy)) +
  geom_point(size = 5) + geom_text(vjust = 2)

# ----- Result ----- #
knnCM
knnaccuracy
dTCM
dTaccuracy
rFCM
rFaccuracy

# ----- Precision, Recall ----- #

precision <- function(cm){
  p.green <- diag(cm)[1] / sum(cm[,1])
  p.orange <- diag(cm)[2] / sum(cm[,2])
  p.red <- diag(cm)[3] / sum(cm[,3])
  p.yellow <- diag(cm)[4] / sum(cm[,4])
  
  r.green <- diag(cm)[1] / sum(cm[1,])
  r.orange <- diag(cm)[2] / sum(cm[2,])
  r.red <- diag(cm)[3] / sum(cm[3,])
  r.yellow <- diag(cm)[4] / sum(cm[4,])
  
  f1.green <- 2 / ((1/p.green) + (1/r.green))
  f1.orange <- 2 / ((1/p.orange) + (1/r.orange))
  f1.red <- 2 / ((1/p.red) + (1/r.red))
  f1.yellow <- 2 / ((1/p.yellow) + (1/r.yellow))
  
  pr <- data.frame('precision' = c(p.green, p.yellow, p.orange, p.red),
                   'recall' = c(r.green, r.yellow, r.orange, r.red),
                   'f1' = c(f1.green, f1.yellow, f1.orange, f1.red))
  return(pr)
}




