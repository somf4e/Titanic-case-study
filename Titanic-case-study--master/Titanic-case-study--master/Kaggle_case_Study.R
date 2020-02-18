#load the data into workspace 

train<-read.csv(file = "train.csv",header = TRUE)
test<-read.csv(file = "test.csv",header = TRUE)

str(train)
summary(train)

table(train$Survived)
prop.table(table(train$Survived))

str(test)
test$Survived<-rep(0,418)

str(test)

Submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
#first submission 
write.csv(Submit,file = "theyallperish.csv",row.names = FALSE)

summary(train$Sex)

prop.table(table(train$Sex,train$Survived))


prop.table(table(train$Sex,train$Survived),1)

test$Survived<-0
test$Survived[test$Sex=='female']<-1

#second submission 

Submit1<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(Submit1,file = "theyallperish.csv",row.names = FALSE)

summary(train$Age)
#create new variable child 
train$Child<-0
train$Child[train$Age< 18]<-1

aggregate(Survived~Child+Sex,data = train,FUN = sum)

#find out the length of people
aggregate(Survived~Child+Sex,data = train,FUN = length)
#find out the proportion
aggregate(Survived~Child+Sex,data = train,FUN = function(x){sum(x)/length(x)})

#divide fare into 3 parts 

summary(train$Fare)
train$Fare2<-'30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20]<-'20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10]<-'10-20'
train$Fare2[train$Fare < 10]<-'10'
str(train)
summary(train$Fare2)
table(train$Fare2)
aggregate(Survived ~ Fare2 + Pclass +Sex ,data = train,FUN = function(x){sum(x)/length(x)})


#Third submission 

test$Survived<-0
test$Survived[test$Sex=='female']<-1
test$Survived[test$Sex=='female'& test$Pclass==3 & test$Fare > 20 ]<- 0

Submit3<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(Submit3,file = "theyallperish.csv",row.names = FALSE)



#fourth submission
#here we are going to use machine learning algorithm
library(rpart)

fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data = train,
           method = "class")
summary(fit)
plot(fit)
text(fit)

#install graphics package to get meaningfull tree

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
prp(fit)

#making prediction
Prediction <-predict(fit,newdata = test,type = "class")

Submit<-data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(Submit,file = "myfirstTreeModel.csv",row.names = FALSE)

#fifth submission 


fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data = train,
           method = "class",
           control = rpart.control(minsplit = 2,cp = 0))
fancyRpartPlot(fit)
prp(fit)

Prediction <-predict(fit,newdata = test,type = "class")

Submit<-data.frame(PassengerId=test$PassengerId,Survived=Prediction)
write.csv(Submit,file = "myfirstTreeModel.csv",row.names = FALSE)


prp(fit)

Prediction <-predict(fit,newdata = test,type = "class")




fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data = train,
           method = "class",
           control = rpart.control(minsplit = 1,cp = 0.01))
Prediction <-predict(fit,newdata = test,type = "class")
mat<-table(test$Survived,Prediction)
prop.table(table(test$Survived,Prediction))

sum(diag(mat))/nrow(test)
prp(fit)





#assign NA to survivied in test data
test$Survived<-NA
train<-read.csv(file = "train.csv",header = TRUE)
combi<-rbind(train,test)
str(combi)
combi$Name<-as.character(combi$Name)
combi$Name[1]

#lets split the name and find out title
strsplit(combi$Name[1],split = '[,.]')
strsplit(combi$Name[1],split = '[,.]')[[1]]
strsplit(combi$Name[1],split = '[,.]')[[1]][2]

#using sapply we can extract all the name title 

combi$Title<-sapply(combi$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][2]})
str(combi)
combi$Title<-sub(' ','',combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme','Mlle')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt','Don','Major','Sir')]<-'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title<-as.factor(combi$Title)
combi$FamiltSize<-combi$SibSp + combi$Parch + 1

#extract surnmae from name 
combi$Surname<-sapply(combi$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})

#combine family size and surname
combi$FamilyID<-paste(as.character(combi$FamiltSize),combi$Surname,sep = "")

combi$FamilyID[combi$FamiltSize <=2]<-'Small'
table(combi$FamilyID)

famIDS<-data.frame(table(combi$FamilyID))

famIDS<-famIDS[famIDS$Freq<=2,]

combi$FamilyID[combi$FamilyID %in% famIDS$Var1]<-'Small'

combi$FamilyID<-as.factor(combi$FamilyID)

trainNew<-combi[1:891,]
testNew<-combi[892:1309,]
str(trainNew)

#preparing new model using new column

fit<-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked 
           + Title + FamiltSize + FamilyID,
           data = trainNew,
           method = "class")
Prediction <-predict(fit,newdata = testNew,type = "class")

prp(fit)

fancyRpartPlot(fit)

Submit<-data.frame(PassengerId=testNew$PassengerId,Survived=Prediction)
write.csv(Submit,file = "myfirstTreeModel2.csv",row.names = FALSE)


