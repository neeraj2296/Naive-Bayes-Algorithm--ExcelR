#inlcuding libraries
#install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(e1071)

#Loading the data set
sal = read.csv(file.choose(), header =T, colClasses = 'factor')

train_sal <- sal#Copying the loaded data as Training Set
str(train_sal)
colnames(train_sal)
#Converting Variables having intger values, as integer varables. 
train_sal$age <- as.integer(train_sal$age)
train_sal$capitalgain <- as.integer(train_sal$capitalgain)
train_sal$capitalloss <- as.integer(train_sal$capitalloss)
train_sal$hoursperweek <- as.integer(train_sal$hoursperweek)
str(train_sal)
View(train_sal)
class(train_sal)

#Loading the test data set.
test_sal <- read.csv(file.choose(), header = T, colClasses = 'factor')
str(test_sal)
#Converting Variables having intger values, as integer varables. 
test_sal$age <- as.integer(test_sal$age)
test_sal$capitalgain <- as.integer(test_sal$capitalgain)
test_sal$capitalloss <- as.integer(test_sal$capitalloss)
test_sal$hoursperweek <- as.integer(test_sal$hoursperweek)
str(test_sal)
View(test_sal)
class(test_sal)

#Visualizing the data Set wrt each other as follows:-

#As BoxPlots,
ggplot(data=train_sal,aes(x=Salary, y = as.integer(age), fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=Salary, y = as.integer(capitalgain), fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=train_sal,aes(x=Salary, y = as.integer(hoursperweek), fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#As BarGraph
plot(train_sal$workclass,train_sal$Salary)
plot(train_sal$education,train_sal$Salary)
plot(train_sal$educationno,train_sal$Salary)
plot(train_sal$maritalstatus,train_sal$Salary)
plot(train_sal$occupation,train_sal$Salary)
plot(train_sal$relationship,train_sal$Salary)
plot(train_sal$race,train_sal$Salary)
plot(train_sal$sex,train_sal$Salary)
plot(train_sal$native,train_sal$Salary)

#As to check number of people having salary above, below and equal to 50k wrt the variables as given
ggplot(data=train_sal,aes(x = age, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt age
ggtitle("Age - Density Plot")
ggplot(data=train_sal,aes(x = workclass, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')# wrt workclas
ggplot(data=train_sal,aes(x = education, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')# wrt education
ggplot(data=train_sal,aes(x = educationno, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt educationno
ggplot(data=train_sal,aes(x = maritalstatus, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')# wrt marital status
ggplot(data=train_sal,aes(x = occupation, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt occupation
ggplot(data=train_sal,aes(x = sex, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt sex
ggplot(data=train_sal,aes(x = relationship, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt relationship
ggplot(data=train_sal,aes(x = race, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt race
ggplot(data=train_sal,aes(x = capitalgain, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt capitalgain
ggplot(data=train_sal,aes(x = capitalloss, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')#wrt capitalloss
ggplot(data=train_sal,aes(x = hoursperweek, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')# wrt hoursperweek
ggplot(data=train_sal,aes(x = native, fill = Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')# wrt native


# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model#Analysing the Model

#Predicting the Salary Numebers using the created model
Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)#Checking the accuracy of the prediction done, i.e. 0.8246348.
confusionMatrix(Model_pred,test_sal$Salary)#Checking the number mistakes done by the model.
