#importing and calling the libraries
library(readxl)
library(caTools)
library(caret)
#importing the data and creating a data drame as data
data<- read_excel("Titanic.xls")
View(data)
str(data)
summary(data)

#Converting the character variable into factor
data$Sex<- as.factor(data$Sex)
data$Cabin<- as.factor(data$Cabin)
data$Embarked<- as.factor(data$Embarked)
str(data)
summary(data)
#removing the 2 column which is name and ticket of the passenger
data<- data[,-c(4,9)]
View(data)
#As cabin variable has many misiing value that is above half of the data is missoing we wont impute it.
data<- data[,-9]
#Replacing the missing value of age with the mean value
data$Age[is.na(data$Age)]<- mean(data$Age, na.rm = TRUE)
data<- na.omit(data)
#converting Pclass and survived to factor
data$Pclass<- as.factor(data$Pclass)
data$Survived<- as.factor(data$Survived)
#finding the co-relation betweenn passengerid, age,sibsp,parch,fare
cor(data[,-c(2,3,4,9)])
data<- data[,-1]
#finding the outlier
boxplot(data$Age)
boxplot(data$SibSp)
boxplot(data$Parch)
boxplot(data$Fare)
quantile(data$Fare, 0.99)
data$Fare[data$Fare>500]<- quantile(data$Fare, 0.99)

#splitting the data #
split<- sample.split(data$Survived, SplitRatio = 0.70)
train<- subset(data, split==TRUE)
test<- subset(data, split==FALSE)


# logistic regression #
model_logistic<- glm(train$Survived~., data = train, family = binomial)
summary(model_logistic)
anova(model_logistic)
pred<- predict(model_logistic, test, type = "response")
View(predictions)
predictions = rep(0, nrow(test))
predictions[pred > 0.5] = 1
predictions<- as.factor(predictions)
confusionMatrix(predictions, test$Survived)
View(test$Survived)

#decision tree #
library(rpart)
library(rpart.plot)
fulltree<- rpart(train$Survived~., data = train, control = rpart.control(cp = 0))
pred_tree<- predict(fulltree, test, type = "class")
confusionMatrix(pred_tree, test$Survived)

# tree pruning #
mincp<- fulltree$cptable[which.min(fulltree$cptable[,"xerror"]), "CP"]
mincp
pruned_tree <- prune(fulltree, cp = mincp)
pred_tree2<- predict(pruned_tree, test, type = "class")
confusionMatrix(pred_tree2, test$Survived)

# considering significant variable#

data<- data[,-c(6,7,8)]
split<- sample.split(data$Survived, SplitRatio = 0.70)
train<- subset(data, split==TRUE)
test<- subset(data, split==FALSE)

# random forest #
library(randomForest)

model_rf<- randomForest(train$Survived~., data = train, ntree= 8) 
pred_rf<- predict(model_rf, test, type = "class")
confusionMatrix(pred_rf, test$Survived)
plot(model_rf)

#As ther variables are very less pruning cannot be done#


# gradient boosting #
library(gbm)
boosting <- gbm(train$Survived~., data = train, distribution = "bernoulli", n.trees = 18,  verbose = F)
pred_boost <- predict(boosting, test, n.trees = 18, type = "response")
predictions5 = rep(0, nrow(test))
predictions5[pred_boost > 0.5] = 1
predictions5<- as.factor(predictions)
confusionMatrix(predictions5, test$Survived)
