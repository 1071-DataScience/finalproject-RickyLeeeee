# Deal with NA

data <- read.csv('./data/AllFeatures+Labelv3.csv', header = TRUE, sep = ',')
AQI <- subset(data, 汽車臺數!=0, select = -c(統計年, 統計月, 縣市別))

sapply(AQI,function(x) sum(x == 0))
car.miss <- which(data$汽車臺數 == 0)
data[car.miss,]

allCity <- levels(data$縣市別)
allMonth <- levels(data$統計月)


