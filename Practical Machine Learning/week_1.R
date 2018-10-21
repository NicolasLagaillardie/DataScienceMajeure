library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list=FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type~., data = training, method = "glm")

set.seed(1235)
modelFit3 <- train(type~., data=training, method="glm")
modelFit3

library(ISLR)
library(ggplot2)
data(Wage)
summary(Wage)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
plot(training)
hist(training$Superplasticizer)
hist(log10(training$Superplasticizer))

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#PCA
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p = 0.75, list=FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
M <- abs(cor(training[,-58]))
diag(M)<-0
which(M>0.8, arr.ind = T)
names(spam)[c(32,34,40)]
plot(spam[,32],spam[,34], col="red")
plot(spam[,32],spam[,40], col='blue')
plot(spam[,40],spam[,34], col='green')

smallSpam <-spam[,c(34,32)]
prComp<-prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

prComp$rotation


preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2])

preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type~., method="glm", data = trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))

modelFit <- train(training$type~., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))

#quizz
#Calculate the number of principal components needed to capture 90% of the variance. 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj


#Help
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

names <- colnames(concrete)
names <- names[-length(names)]

featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
