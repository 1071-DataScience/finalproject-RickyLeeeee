#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages('ROCR')


library('ROCR')
library("rpart")
library("rpart.plot")
library("rattle")

data <- read.csv('./data/AllFeatures+Labelv3.csv', header = TRUE, sep = ',')
data[,5:ncol(data)] <- scale(data[,5:ncol(data)], center = TRUE, scale = TRUE)

# Remove unused columns
AQI <- subset(data, 汽車臺數!=0, select = -c(統計年, 統計月, 縣市別))
#AQI$AQI <- ifelse(AQI$AQI == 'green', 1, 0)

n <- nrow(AQI)
set.seed(1111)
# Randomly shuffle the data
newAQI <- AQI[sample(n),]

# Split into training and testing data
t_idx <- sample(seq_len(n), size = round(0.7 * n))
traindata <- newAQI[t_idx,]
testdata <- newAQI[ - t_idx,]

# Construct the decision Tree Model
dTreeModel <- rpart(formula = AQI ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001))
fancyRpartPlot(dTreeModel)

# Prediction
dTresult <- predict(dTreeModel, newdata = testdata, type = "class")
dTCM <- table(testdata$AQI, dTresult, dnn = c("實際", "預測"))
dTCM
dTaccuracy <- sum(diag(dTCM)) / sum(dTCM)
dTaccuracy

# Plotting the decision tree
print(dTreeModel)
pdf("decisionTree.pdf")
par(cex=0.7)
plot(dTreeModel)
text(dTreeModel)
dev.off()

