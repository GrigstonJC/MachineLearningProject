require(caret)
require(doMC)

registerDoMC(cores=1)

data <- read.csv('pml-training.csv')
data <- data[, colSums(is.na(data)) == 0]
data <- data[, -nearZeroVar(data)]
data <- data[ , -which(names(data) %in% c('X', 'user_name', 'raw_timestamp_part_1',
                                          'raw_timestamp_part_2', 'cvtd_timestamp',
                                          'num_window'))]

set.seed(1357)
inTrain <- createDataPartition(y=data$classe, p = 0.75, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

tc <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
modFit.rf <- train(classe ~ ., data = training,
                   preProcess = c('scale', 'center'), ###
                   trControl = tc,
                   nTrees = 10,
                   verbose = TRUE,
                   method = 'rf')
modPredict.rf <- predict(modFit.rf, testing)
conMatrix.rf <- confusionMatrix(testing$classe, modPredict.rf)
imp.rf <- varImp(modFit.rf, scale=TRUE)