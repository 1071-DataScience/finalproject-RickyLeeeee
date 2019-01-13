require(ROCR)
require(rpart)
require(rpart.plot)
require(rattle)
require(randomForest)
require(class)
require(dplyr)
require(ggplot2)


# Read the data and scale the data
rawData <- read.csv('./data/AllFeatures+Labelv4.csv', header = TRUE, sep = ',')
rawData[,5:ncol(rawData)] <- scale(rawData[,5:ncol(rawData)], center = TRUE, scale = TRUE)

# Remove unused columns
# k = 49
# data <- subset(rawData, 汽車臺數!=0, select = -c(統計年, 統計月, 縣市別, 罰營建工程次數, 罰其他固定污染源次數,
# 檢營建工程次數, 罰移動污染源次數, 人均日廢棄物KG, 檢其他固定污染源次數))
# k = 6
data <- subset(rawData, 汽車臺數!=0, select = c(AQI, 汽車臺數, 機車臺數))

#AQI$AQI <- ifelse(AQI$AQI == 'green', 1, 0)

# Randomly shuffle the data
data.row <- nrow(data)
set.seed(1111)
shuffleData <- data[sample(data.row),]
data.Labels <- shuffleData$AQI


# ----- Initialize the variable ----- #
type <- c('green', 'orange', 'red', 'yellow')
measurement <- c('p', 'r', 'f1')
model <- c('dT', 'rF', 'knn')
for(i in model){
  for(j in measurement){
    tmp <- c()
    for(k in type){
      nam <- paste(i, j ,k, sep = ".")
      tmp <- append(tmp, k)
    }
    #mnam <- paste('m', i, j, sep = ".")
    #assign(mnam, tmp)
    dfnam <- paste('measure', i, j, sep = ".")
    modelnam <- paste(i, j, sep = "-")
    #assign(dfnam, data.frame(matrix(0, ncol = length(tmp), nrow = 1)))
    assign(dfnam, data.frame(model=modelnam, type=tmp, value=0))
  }  
}


knn.accu <- 0
dT.accu <- 0
rF.accu <- 0
avg.knn.accu <- 0
avg.dT.accu <- 0
avg.rF.accu <- 0


# ----- Precision, Recall, f1 ----- #
precision <- function(cm, n){
  p <- diag(cm)[n] / sum(cm[,n])
  return (p)
}

recall <- function(cm, n){
  r <- diag(cm)[n] / sum(cm[n,])
  return (r)
}

f1 <- function(cm, n){
  f <- 2 / ((1/precision(cm, n)) + (1/recall(cm, n)))
  return (f)
}


# ----- n fold ----- #
# -- Create n equally size folds -- #
n <- 5
folds <- cut(seq(1, data.row), breaks = n, labels = FALSE)

for(i in c(1:n)){
  cat('Now executing the fold', i, '...\n')
  
  # -- Split the data set -- #
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- shuffleData[testIndexes, ]
  test.Label <- data.Labels[testIndexes]
  
  trainData <- shuffleData[-testIndexes, ]
  train.Label <- data.Labels[-testIndexes]
  
  # -- Decision Tree -- #
  dT.Model <- rpart(formula = AQI ~ ., data = trainData, method = "class", control = rpart.control(cp = 0.001))
  dT.Result <- predict(dT.Model, newdata = testData, type = "class")
  dT.CM <- table(test.Label, dT.Result, dnn = c("實際", "預測"))
  dT.accu <- dT.accu + sum(diag(dT.CM)) / sum(dT.CM)
  for(m in 1:4){
    measure.dT.p[m,3] <- measure.dT.p[m,3] + precision(dT.CM, m)
    measure.dT.r[m,3] <- measure.dT.r[m,3] + recall(dT.CM, m)
    measure.dT.f1[m,3] <- measure.dT.f1[m,3] + f1(dT.CM, m)
  }
  
  # -- Random Forest -- #
  rF.Model <- randomForest(AQI ~ ., data = trainData, importane = T, proximity = T, do.trace = 100)
  rF.Result <- predict(rF.Model, newdata = testData, type = "class")
  rF.CM <- table(test.Label, rF.Result, dnn = c("實際", "預測"))
  rF.accu <- rF.accu + sum(diag(rF.CM)) / sum(rF.CM)
  for(m in 1:4){
    measure.rF.p[m,3] <- measure.rF.p[m,3] + precision(rF.CM, m)
    measure.rF.r[m,3] <- measure.rF.r[m,3] + recall(rF.CM, m)
    measure.rF.f1[m,3] <- measure.rF.f1[m,3] + f1(rF.CM, m)
  }

  # -- KNN -- #
  knnTrain <- subset(trainData, select = -c(AQI))
  knnTest <- subset(testData, select = -c(AQI))
  knn.Result <- knn(train = knnTrain, test = knnTest, cl = train.Label, k = 6)
  knn.CM <- table(test.Label, knn.Result, dnn = c("實際", "預測"))
  knn.accu <- knn.accu + sum(diag(knn.CM)) / sum(knn.CM)
  for(m in 1:4){
    measure.knn.p[m,3] <- measure.knn.p[m,3] + precision(knn.CM, m)
    measure.knn.r[m,3] <- measure.knn.r[m,3] + recall(knn.CM, m)
    measure.knn.f1[m,3] <- measure.knn.f1[m,3] + f1(knn.CM, m)
  }
}


# ----- Average ----- #
for(m in 1:4){
  measure.dT.p[m,3] <- measure.dT.p[m,3] / n
  measure.dT.r[m,3] <- measure.dT.r[m,3] / n
  measure.dT.f1[m,3] <- measure.dT.f1[m,3] / n
  measure.rF.p[m,3] <- measure.rF.p[m,3] / n
  measure.rF.r[m,3] <- measure.rF.r[m,3] / n
  measure.rF.f1[m,3] <- measure.rF.f1[m,3] / n
  measure.knn.p[m,3] <- measure.knn.p[m,3] / n
  measure.knn.r[m,3] <- measure.knn.r[m,3] / n
  measure.knn.f1[m,3] <- measure.knn.f1[m,3] / n
}
avg.dT.accu <- dT.accu / n
avg.rF.accu <- rF.accu / n
avg.knn.accu <- knn.accu / n


# ----- Choose best k value ----- #


klist <- seq(1:sqrt(data.row))
knnFunction <- function(x, knnTrain, knnTest, trainLabels, testLabels) {
  prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = x)
  cm <- table(x = testLabels, y = prediction)
  accuracy <- sum(diag(cm)) / sum(cm)
}
accuracies <- sapply(klist, knnFunction, knnTrain = knnTrain, knnTest = knnTest, trainLabels = train.Label, testLabels = test.Label)
# -- k value visualization -- #
df <- data.frame(kv = klist, accuracy = accuracies)
ggplot(df, aes(x = kv, y = accuracy, label = kv, color = accuracy)) +
  geom_point(size = 5) + geom_text(vjust = 2)






# test <- function(cm){
#   p.green <- diag(cm)[1] / sum(cm[,1])
#   p.orange <- diag(cm)[2] / sum(cm[,2])
#   p.red <- diag(cm)[3] / sum(cm[,3])
#   p.yellow <- diag(cm)[4] / sum(cm[,4])
#   
#   r.green <- diag(cm)[1] / sum(cm[1,])
#   r.orange <- diag(cm)[2] / sum(cm[2,])
#   r.red <- diag(cm)[3] / sum(cm[3,])
#   r.yellow <- diag(cm)[4] / sum(cm[4,])
#   
#   f1.green <- 2 / ((1/p.green) + (1/r.green))
#   f1.orange <- 2 / ((1/p.orange) + (1/r.orange))
#   f1.red <- 2 / ((1/p.red) + (1/r.red))
#   f1.yellow <- 2 / ((1/p.yellow) + (1/r.yellow))
#   
#   pr <- data.frame('precision' = c(p.green, p.yellow, p.orange, p.red),
#                    'recall' = c(r.green, r.yellow, r.orange, r.red),
#                    'f1' = c(f1.green, f1.yellow, f1.orange, f1.red))
#   return(pr)
# }


# Plotting the decision tree
#fancyRpartPlot(dTModel)
#print(dTModel)
#pdf("decisionTree.pdf")
#par(cex=0.7)
#plot(dTModel)
#text(dTModel)
#dev.off()


p <- ggplot(data=measure.dT.f1, aes(x=type, y=value)) +
  geom_bar(stat="identity", width = 0.5, fill = 'orange', color = 'black')+
  geom_text(aes(label=value), vjust=-1, size=3.5)+
  theme_minimal()
p

