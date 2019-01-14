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
model <- c('dt', 'rf', 'knn')
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
dt.accu <- 0
rf.accu <- 0
avg.knn.accu <- 0
avg.dt.accu <- 0
avg.rf.accu <- 0


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
  dt.Model <- rpart(formula = AQI ~ ., data = trainData, method = "class", control = rpart.control(cp = 0.001))
  dt.Result <- predict(dt.Model, newdata = testData, type = "class")
  dt.CM <- table(test.Label, dt.Result, dnn = c("實際", "預測"))
  dt.accu <- dt.accu + sum(diag(dt.CM)) / sum(dt.CM)
  for(m in 1:4){
    measure.dt.p[m,3] <- measure.dt.p[m,3] + precision(dt.CM, m)
    measure.dt.r[m,3] <- measure.dt.r[m,3] + recall(dt.CM, m)
    measure.dt.f1[m,3] <- measure.dt.f1[m,3] + f1(dt.CM, m)
  }
  
  # -- Random Forest -- #
  rf.Model <- randomForest(AQI ~ ., data = trainData, importane = T, proximity = T, do.trace = 100)
  rf.Result <- predict(rf.Model, newdata = testData, type = "class")
  rf.CM <- table(test.Label, rf.Result, dnn = c("實際", "預測"))
  rf.accu <- rf.accu + sum(diag(rf.CM)) / sum(rf.CM)
  for(m in 1:4){
    measure.rf.p[m,3] <- measure.rf.p[m,3] + precision(rf.CM, m)
    measure.rf.r[m,3] <- measure.rf.r[m,3] + recall(rf.CM, m)
    measure.rf.f1[m,3] <- measure.rf.f1[m,3] + f1(rf.CM, m)
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
  measure.dt.p[m,3] <- measure.dt.p[m,3] / n
  measure.dt.r[m,3] <- measure.dt.r[m,3] / n
  measure.dt.f1[m,3] <- measure.dt.f1[m,3] / n
  measure.rf.p[m,3] <- measure.rf.p[m,3] / n
  measure.rf.r[m,3] <- measure.rf.r[m,3] / n
  measure.rf.f1[m,3] <- measure.rf.f1[m,3] / n
  measure.knn.p[m,3] <- measure.knn.p[m,3] / n
  measure.knn.r[m,3] <- measure.knn.r[m,3] / n
  measure.knn.f1[m,3] <- measure.knn.f1[m,3] / n
}
avg.dt.accu <- dt.accu / n
avg.rf.accu <- rf.accu / n
avg.knn.accu <- knn.accu / n


# ----- Choose best k value ----- #

# klist <- seq(1:sqrt(data.row))
# knnFunction <- function(x, knnTrain, knnTest, trainLabels, testLabels) {
#   prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = x)
#   cm <- table(x = testLabels, y = prediction)
#   accuracy <- sum(diag(cm)) / sum(cm)
# }
# accuracies <- sapply(klist, knnFunction, knnTrain = knnTrain, knnTest = knnTest, trainLabels = train.Label, testLabels = test.Label)
# # -- k value visualization -- #
# df <- data.frame(kv = klist, accuracy = accuracies)
# ggplot(df, aes(x = kv, y = accuracy, label = kv, color = accuracy)) +
#   geom_point(size = 5) + geom_text(vjust = 2)


# Plotting the decision tree
#fancyRpartPlot(dTModel)
#print(dTModel)
#pdf("decisionTree.pdf")
#par(cex=0.7)
#plot(dTModel)
#text(dTModel)
#dev.off()


# ----- Visualization ----- #
dt.combine <- rbind(measure.dt.f1,measure.dt.p,measure.dt.r)
rf.combine <- rbind(measure.rf.f1,measure.rf.p,measure.rf.r)
knn.combine <- rbind(measure.knn.f1,measure.knn.p,measure.dt.r)

plot <- function(a){
  if(as.character(substitute(a)) == 'knn.combine'){
    title <- 'KNN'
  } else if(as.character(substitute(a)) == 'rf.combine'){
    title <- 'Random Forest'
  } else {
    title <- 'Decision Tree'
  }
  p <- ggplot(data=a, aes(reorder(x=type, -value), y=value, fill=model)) +
    ggtitle(title) +
    xlab("Type")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_bar(stat="identity", width = 0.5, position=position_dodge())+
    geom_text(aes(label=round(value,2)), vjust=+0.3, 
              position = position_dodge(0.5), size=5)+
    scale_fill_brewer(palette="Set1")+
    coord_flip()
  p
}
plot(dt.combine)
#--
AQIbar <- as.data.frame(table(rawData$AQI))

p2 <- ggplot(data=AQIbar, aes(reorder(x=Var1, -Freq), y=Freq)) +
  ggtitle("AQI") +
  xlab("Var1")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(stat="identity", width = 0.5, fill = "Orange")+
  geom_text(aes(label=Freq), vjust=-0.3)+
  scale_fill_brewer(palette="Dark2")

p2

