library(class)
library(dplyr)

data <- read.csv('./data/replaceChinese.csv', header = TRUE, sep = ',')


# Remove unused columns
AQI <- subset(data, select = -c(Year, Month, City))
#AQI$AQI <- ifelse(AQI$AQI == 'green', 1, 0)
n <- nrow(AQI)
set.seed(1111)
# Randomly shuffle the data
newAQI <- AQI[sample(n),]


# Split into training and testing data
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- newAQI[t_idx,]
testdata <- newAQI[ - t_idx,]

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

