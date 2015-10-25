# Week 3 Quiz
# Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# 1. Subset the data to a training set and testing set based on the Case variable
# in the data set. 
training <- segmentationOriginal[segmentationOriginal$Case == "Train", ]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test", ]

# 2. Set the seed to 125 and fit a CART model with the rpart method using all 
# predictor variables and default caret settings. 
set.seed(125)
modFit <- train(Class ~ ., method ="rpart", data = training)

# 3. In the final model what would be the final model prediction for cases with 
# the following variable values:
modFit$finalModel
# n= 1009 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 1) root 1009 373 PS (0.63032706 0.36967294)  
# 2) TotalIntenCh2< 45323.5 454  34 PS (0.92511013 0.07488987) *
# 3) TotalIntenCh2>=45323.5 555 216 WS (0.38918919 0.61081081)  
# 6) FiberWidthCh1< 9.673245 154  47 PS (0.69480519 0.30519481) *
# 7) FiberWidthCh1>=9.673245 401 109 WS (0.27182045 0.72817955) *

# In order to use fancyRpartPlot function, we need to install rattle package 
# and rpart.plot package first
library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
# ---a. PS 
# ---b. WS 
# ---c. PS
# ---d. Not possible to predict

# Question 2
# NOTE: larger k = less bias, more variance
#       smaller k = more bias, less variance

# If K is small in a K-fold cross validation, is the bias in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger?
# ---more bias(smaller accuracy)

# If K is small, is the variance in the estimate of out-of-sample (test set) 
# accuracy smaller or bigger?
# ---less variance

# Is K large or small in leave one out cross validation?
# ---K is large.

# ---The bias is larger and the variance is smaller. Under leave one out 
# ---cross validation K is equal to the sample size.

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]

modFit <- train(Area ~ ., method ="rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit,newdata) # 2.783282
# ---2.783. It is strange because Area should be a qualitative variable 
# ---but tree is reporting the average value of Area as a numeric variable in 
# ---the leaf predicted for newdata

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
# Build a logistic regression model
set.seed(13234)
logitMol <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,
                method="glm",family="binomial")
# Calculate misclassification rate
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# Test Set Misclassification
missClass(testSA$chd, predict(logitMol, newdata = testSA)) # 0.3116883
# Training Set Misclassification
missClass(trainSA$chd, predict(logitMol, newdata = trainSA)) # 0.2727273
# ---Test Set Misclassification: 0.31 
# ---Training Set: 0.27

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
rfMol <- train(y ~ ., data=vowel.train, method="rf")
# Calculate the variable importance
varImp(rfMol)
# ---The order of the variables is:
# ---x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10