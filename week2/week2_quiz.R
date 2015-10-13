# Week 2 Quiz
# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a histogram
hist(training$Superplasticizer)
# ---There are values of zero so when you take the log() transform those values
# ---will be -Inf.

# Question 3
# Calculate the number of principal components needed to capture 90% of the 
# variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Find all the predictor variables in the training set that begin with IL
names(training)
IL_predictors <- training[,grep("^IL",x=names(training))]
names(IL_predictors)
preProcess(IL_predictors,method = "pca",thresh = 0.9)
# Pre-processing:
#         - centered (12)
# - principal component signal extraction (12)
# - scaled (12)
# PCA needed 9 components to capture 90 percent of the variance
# Answer: 9

# Question 4
# Build two predictive models, one using the predictors as they are and one using 
# PCA with principal components explaining 80% of the variance in the predictors. 
# Use method="glm" in the train function. What is the accuracy of each method in
# the test set? Which is more accurate?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Select all predictors begin with "IL"
df <- adData[,c(1,58:69)]
# Construct new training and testing set
training_new <-df[inTrain, ]
testing_new <-df[-inTrain, ]
# Construct model1 using the predictors as they are
model1 <- train(diagnosis ~., method="glm", data=training_new)
# Result of model1
result1 <- confusionMatrix(testing_new$diagnosis,predict(model1, testing_new))
result1$overall[1]
# ---Accuracy 
# ---0.6463415

# Construct model2
# Construct model2 using PCA
model2 <- train(training_new$diagnosis ~ ., method="glm",
                preProcess="pca", data = training_new,
                trControl=trainControl(preProcOptions=list(thresh=0.8)))
# Result of model2
result2 <- confusionMatrix(testing_new$diagnosis, predict(model2, testing_new))
result2$overall[1]
# ---Accuracy 
# ---0.7195122