library(randomForest)
set.seed(1117)

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


# Construct the Random Foreset Model
rForestModel <- randomForest(AQI ~ ., data = traindata, importane = T, proximity = T, do.trace = 100)
rForestModel
plot(rForestModel)


#Prediction
rFresult <- predict(rForestModel, newdata = testdata, type = "class")
rFCM <- table(testdata$AQI, rFresult, dnn = c("實際", "預測"))
rFCM
rFaccuracy <- sum(diag(rFCM)) / sum(rFCM)
rFaccuracy


