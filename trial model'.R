rm(list = ls())
#getting the data
train = read.csv('train.csv', stringsAsFactors = F, header = T)
test = read.csv('test.csv', stringsAsFactors =  F, header = T)
train$istrainset = T
test$istrainset = F
test$Survived = NA

full = rbind(train,test) #combining both datasets
str(full)

#cleaning the data


full[full$Embarked=='', 'Embarked'] = 'S' #removing missing values from embarked
#full$Age[is.na(full$Age)] = median(full$Age, na.rm = T) #removing missing values from age
#full$Fare[is.na(full$Fare)] = median(full$Fare, na.rm = T) #removing missing values from fare 

#replacing the NA values in Fare by predicting using linear regression model
limit = boxplot.stats(full$Fare)$stats[5] #limits the outlier
outlier.filter = full$Fare < limit #filter outs the outliers
full[outlier.filter,] #gives only rows within the range
fare.equation = 'Fare ~ Pclass+Sex+Age+SibSp+Parch+Embarked'
fare.model = lm(formula = fare.equation,data = full[outlier.filter,]) #linear regression model
fare.row = full[is.na(full$Fare),c("Pclass", "Sex","Age","SibSp","Parch","Embarked")]
fare.pred = predict(fare.model,newdata = fare.row )
full[is.na(full$Fare),'Fare'] = fare.pred

#replacing the NA values in Age by predicting using linear regression model
limit1 = boxplot.stats(full$Age)$stats[5]
outlier.filter1 = full$Age < limit1
full[outlier.filter1,]
age.equation = 'Age ~ Pclass+Sex+Fare+SibSp+Parch+Embarked'
age.model = lm(formula = age.equation , data = full[outlier.filter1,])
age.row = full[is.na(full$Age),c("Pclass", "Sex","Fare","SibSp","Parch","Embarked")]
age.pred = predict(age.model, newdata = age.row)
full[is.na(full$Age),'Age'] = age.pred





table(is.na(full$Survived))
str(full)
full$Pclass = as.factor(full$Pclass)
full$Sex = as.factor(full$Sex)
full$Embarked = as.factor(full$Embarked)

#splitting back the data
train = full[full$istrainset==T,]
test = full[full$istrainset ==F,]
tail(train)
train$Survived = as.factor(train$Survived) #converting survived to factor as it will tell its binary classification problem

#building model
library(randomForest)
equation = 'Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked'
formula1 = as.formula(equation)
model = randomForest(formula = formula1, data = train,ntree = 500, mtry =3)

#prediction
pred = predict(model,test)
pred

#creating the solution
solution <- data.frame(PassengerID = test$PassengerId, Survived = pred)
write.csv(solution, file = 'SolutionbyRF.csv', row.names = F)
