 ##
 # This approach uses random forest method for prediction 
 #setwd("E:\\unni\\caret")
 
 trainingData = read.csv("pml-training.csv", na.strings=c("", "NA", "NULL"))
 testingData = read.csv("pml-testing.csv", na.strings=c("", "NA", "NULL"))
 
 training.tmp <- trainingData[ , colSums(is.na(trainingData)) == 0]
 filterCol = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
 training.filtered <- training.tmp[, -which(names(training.tmp) %in% filterCol)]
 
 # loading caret package
 library(caret)
 
 zeroVar = nearZeroVar(training.filtered[sapply(training.dere, is.numeric)], saveMetrics = TRUE)
 training.nonzerovar = training.tmp[,zeroVar[, 'nzv']==0]
 corrMatrix <- cor(na.omit(training.nonzerovar[sapply(training.nonzerovar, is.numeric)]))
 
 corrDF <- expand.grid(row = 1:52, col = 1:52)
 corrDF$correlation <- as.vector(corrMatrix)
 ### find correlation matrix
 fcor = findCorrelation(corrMatrix, cutoff = .90, verbose = TRUE)
 training.cor = training.nonzerovar[,-fcor]
 inTrain  <- createDataPartition(y=training.cor$classe, p=0.7, list=FALSE)
 training <- training.cor[inTrain,] 
 testing  <- training.cor[-inTrain,]
 
 #### use Random Forest approach
 require(randomForest)
 set.seed(12345)
 #### training data
 rf.training=randomForest(classe~.,data=training,ntree=100, importance=TRUE)
 
 ### does the prediction on testing data
 tree.pred  = predict(rf.training, testing, type="class")
 predMatrix = with(testing, table(tree.pred, classe))
 sum(diag(predMatrix))/sum(as.vector(predMatrix)) # this will produce 0.99 value so it can be accepted 
 answers <- predict(rf.training, testingData)
 ###
 answers
 
