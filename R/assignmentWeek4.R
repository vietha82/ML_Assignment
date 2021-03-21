library(data.table); library(ggplot2); library(caret); library(dplyr);library(corrplot);library(randomForest)
setwd("C:/Users/asian/Downloads")
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 'training.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 'testing.csv')

trainingData<-read.csv('training.csv')
testingData<-read.csv('testing.csv')
#first 7 columns of the data sets are not numberic measurements but rather subject identifier and time stamps
trainingData<-trainingData %>% select(!c(1:7))
testingData<-testingData %>% select(!c(1:7))

#due to computer time remove all near zero variance (95%)
#https://www.rdocumentation.org/packages/caret/versions/6.0-86/topics/nearZeroVar
NZV<-nearZeroVar(trainingData)
trainingData<-trainingData[,-NZV]
testingData<-testingData[,-NZV]
#lots of NA values so remove columns that are all NA
trainingData<-trainingData[, colSums(is.na(trainingData)) == 0]
testingData<-testingData[, colSums(is.na(testingData)) == 0]
trainingData$classe<-factor(trainingData$classe)
testingData$problem_id<-factor(testingData$problem_id)
#partition
inTrain<-createDataPartition(y=trainingData$classe, p=0.7, list=F)
training<-trainingData[inTrain,]
testing<-trainingData[-inTrain,]

#Exploratory Analysis. Darker colors have higher correlation but not too many here
corData <- cor(training[,-53])
corrplot(corData, method = "color", type = "lower", tl.cex = 0.5)
#run models: decision tree, random forest,
mod1<-randomForest(classe~., data=training, method='class')#random forest model
mod2<-train(classe ~ .,  data=training, method="rpart",  trControl = trainControl(method = "cv"))#decision tree model
#generalized boosting
fitcontrol <- trainControl(method="cv",number=5,allowParallel=TRUE)
#Gradient boosting
mod3 <- train(classe ~ ., data = training, method = "gbm", trControl = fitcontrol, 
                   verbose = FALSE, na.action = na.omit)

#predictions
pred1<-predict(mod1, testing)
pred2<-predict(mod2, testing)
pred3<-predict(mod3, testing)
#confusion matrix
accuracy1<-confusionMatrix(pred1, testing$classe)
accuracy2<-confusionMatrix(pred2, testing$classe)
accuracy3<-confusionMatrix(pred3, testing$classe)
accuracy1$overall['Accuracy']
accuracy2$overall['Accuracy']
accuracy3$overall['Accuracy']

#importance plots
importance1<-varImp(mod1, scale = FALSE)
importance2<-varImp(mod2, scale = FALSE)
importance3<-varImp(mod3, scale = FALSE)
plot(importance2)

#final prediction on testing set
predict(mod1, testingData)
predict(mod3,testingData)
#exactly according to the specification (Class A), throwing the elbows to the front (Class B), 
#lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and 
#throwing the hips to the front (Class E).
