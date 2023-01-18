
### Decision Trees ###
data <- loan_sanction
length(data[,1])                 
View(data)
head(data)

data <- data[-1]


### Required Packages ###
library(mice)

install.packages("rpart")
library(rpart)

install.packages("rattle")
library(rattle)

### Missing Values ###
md.pattern(data)
data_new <- na.omit(data)
nrow(data)
nrow(data_new)

ındex <- which(data_new$Gender == "")
data_new <- data_new[-ındex,]

### Transformation Variables ###
str(data_new)

data_new$Gender <- as.factor(data_new$Gender)
data_new$Married <- as.factor(data_new$Married)
data_new$Dependents <- as.factor(data_new$Dependents)
data_new$Education <- as.factor(data_new$Education)
data_new$Self_Employed <- as.factor(data_new$Self_Employed)
data_new$Property_Area <- as.factor(data_new$Property_Area)
data_new$Loan_Status <- as.factor(data_new$Loan_Status)


### Splitting Dataset ###
sampleLength <- table(data_new$Loan_Status)[2]-table(data_new$Loan_Status)[1]

sampleindex <- sample(which(data_new$Loan_Status == "Y"),
                      size = sampleLength)

data_new <- data_new[-sampleindex , ]

names(data_new)
table(data_new$Loan_Status)

set.seed(20)
trainIndex <- sample(1:nrow(data_new) , size = 0.8*nrow(data_new))

trainSet <- data_new[trainIndex,]
testSet <- data_new[-trainIndex,]

nrow(trainSet)
nrow(testSet)

table(trainSet$Loan_Status)


### Creating Model ###
modelEntropy <- rpart(Loan_Status ~ . , data = trainSet , method = "class" , 
                      parms = list(split = "information"))

modelGini <- rpart(Loan_Status ~ . , data = trainSet , method = "class" , 
                   parms = list(split = "gini"))

modelEntropy
fancyRpartPlot(modelEntropy)
summary(modelEntropy)

modelGini
fancyRpartPlot(modelGini)
summary(modelGini)


### Prediction ###
predModelEntropy <- predict(modelEntropy , testSet , type = "class")
predModelGini <- predict(modelGini , testSet , type = "class")

library(caret)

confusionMatrix(predModelEntropy , testSet$Loan_Status)
confusionMatrix(predModelEntropy , testSet$Loan_Status , mode = "prec_recall")
confusionMatrix(predModelEntropy , testSet$Loan_Status , mode = "prec_recall" , positive = "Y")

confusionMatrix(predModelGini , testSet$Loan_Status)
confusionMatrix(predModelGini , testSet$Loan_Status , mode = "prec_recall")
confusionMatrix(predModelGini , testSet$Loan_Status , mode = "prec_recall" , positive = "Y")


### Model Tuning ###
modelLookup("rpart")
modelLookup("rpart2")
library(e1071)

trControl <- trainControl(method="cv" , number = 3 , search = "random")
trControl2 <- trainControl(method="cv" , number = 3 , search = "grid")

modelCP <- train(Loan_Status ~ . , data  = trainSet ,
                 method = "rpart", tuneLength = 20,
                 trControl = trControl)
modelCP
modelCP$finalModel

modelMD <- train(Loan_Status ~ . , data  = trainSet ,
                 method = "rpart2", tuneLength = 20,
                 trControl = trControl)
modelMD$finalModel