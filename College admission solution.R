set.seed(1)

#setting the working directory
setwd("C:/Users/PC/Desktop/R programming samples/projects/college admission logistic")

#loading the dataset
dataset<-read.csv("College_admission.csv")

#viewing the dataset
View(dataset)
dataset
head(dataset)

#Finding the missing values
is.na(dataset)
which(is.na(dataset))
# there is no missing values

#finding the outlier
#Boxplot can be used for finding outliers
#Those outliers must be removed
boxplot(dataset$gre)
dataset<-dataset[dataset$gre>320,]
boxplot(dataset$gre)

boxplot(dataset$gpa)
dataset<-dataset[dataset$gpa>2.5,]
boxplot(dataset$gpa)

#summary of the exsisting data
str(dataset)
summary(dataset)
colnames(dataset)

#transforming the numeric data to factor
dataset$ses<-as.factor(dataset$ses)
dataset$Gender_Male<-as.factor(dataset$Gender_Male)
dataset$Race<-as.factor(dataset$Race)
dataset$admit<-as.factor(dataset$admit)
dataset$rank<-as.factor(dataset$rank)
str(dataset)


#Normalizing tha data
qqnorm(dataset$gre)

qqnorm(dataset$gpa)
#Exsisting data is perfectly normalized

#univariate graphs
table(dataset$admit)
count_admit<-c(table(dataset$admit))
barplot(sort(count_admit),main="Number of admitted students",xlab=" 1- admitted, 0-not admitted",col="green")


table(dataset$ses)
count_ses=table(dataset$ses)
barplot((count_ses),main="Socio economic status of students",xlab="1 - low, 2 - medium, 3 - high",col="black")

table(dataset$Gender_Male)
count_Gender_Male<-c(table(dataset$Gender_Male))
barplot((count_Gender_Male),main="Gender of students",xlab="0 -> Female, 1 -> Male",col="red")


table(dataset$Race)
count_Race<-c(table(dataset$Race))
barplot((count_Race),main="Race of students",xlab="1-Hispanic, 2-Asian, and 3-African-American ",col="yellow")


table(dataset$rank)
count_rank<-c(table(dataset$rank))
barplot((count_rank),main="Prestige rank of their undergraduate institution",xlab="Prestige rate from 1 to 4",col="blue")




#logistic model to determine the factors that influence the admission process of a student

#dividing the data into train test split
n=length(dataset$admit)
n1<-floor(n*0.6)
train=sample(1:n,n1)     

traindata<- dataset[train,]
testdata<-dataset[-train,]

#applying logistic regression
logistic_result=glm(admit~.,family=binomial,data=traindata)
summary(logistic_result)

logistic_prd=predict(logistic_result, newdata=testdata)
logistic_prd
cm_logistic=table(testdata$admit,logistic_prd>0.5)
cm_logistic

#from the logistic regression we can eliminate the variables which are not significant
#It was found that gender, race and ses does not contribute much to prediction. Hence we can delete those variables.
traindata<-traindata[,c(-4,-5,-6)]
testdata<-testdata[,c(-4,-5,-6)]

result2=glm(admit~.,family=binomial,data=traindata)
summary(result2)

prd2=predict(result2, newdata=testdata)
prd2

cm2=table(testdata$admit,prd2>0.5)
cm2

library(pROC)
#Evaluation of model using AUC-ROC Curve
curve2<-roc(testdata$admit,as.numeric(prd2))
curve2
curve2_auc=auc(curve2)
plot(curve2)
#Accuracy of the logistic regression is 65.60


#Applying SVM algorithm
library(e1071)

result3=svm(admit~.,family=binomial,data=traindata)
summary(result3)

prd3=predict(result3, newdata=testdata)
prd3
cm3=table(testdata$admit,prd3)
cm3
curve3<-roc(testdata$admit,as.numeric(prd3))
curve3
curve3_auc=auc(curve3)
plot(curve3)
#Accuracy of the Svm algorithm is 65.60


#Applying decision tree algorithm

library(rpart)
library(rpart.plot)
result4=rpart(admit~.,data=traindata,cp=0,minbucket=3)
summary(result4)
result4
rpart.plot(result4)
printcp(result4)

#we have to cut the tree at appropriate position
result5=rpart(admit~.,data=traindata,method="class",cp=0.0217391,minbucket=5)
result5
rpart.plot(result5)
prd5=predict(result5, newdata=testdata)
prd5
cm5=table(testdata$admit,prd5>0.5)
cm5
curve5<-roc(testdata$admit,as.numeric(prd5))
curve5
curve5_auc=auc(curve5)
plot(curve5)
#Accuracy of the tree algorithm is 67.51



#Applying random forest algorithm

library(randomForest)

library(caTools)

result6=randomForest(admit~.,data=traindata,ntree=100,mtry=4)
result6
plot(result6)
prd6=predict(result6, newdata=testdata)
prd6
cm6=table(testdata$admit,prd6)
cm6
curve6<-roc(testdata$admit,as.numeric(prd6))
curve6
curve6_auc=auc(curve6)
plot(curve6)
#Accuracy of the random forest algorithm is 66.24


#It was found that decision tree algorithm provides highest accuracy of 67.51 compared to all other algorithms. 
# Hence decision tree model is chosen

#New data set contain only admitted sutdents.
newdataset<-filter(dataset,admit==1)
newdataset

library(dplyr)
#creating new variable for catagorizing the gre marks of the students.
newdataset<-mutate(newdataset,newgre="Medium")
newdataset$newgre[newdataset$gre<440]<-"Low"
newdataset$newgre[newdataset$gre>580]<-"High"
table(newdataset$newgre)
count<-c(table(newdataset$newgre))

#ploting the gre marks of the admitted students using bar plot

barplot(sort(count),main="Gre mark rate of admitted students",xlab="Gre marks")
Low<-newdataset$gre[newdataset$gre<440]
Low
high<-newdataset$gre[newdataset$gre>580]
medium<-newdataset$gre[!newdataset$gre>580 & !newdataset$gre<440]

#ploting the gre marks of the admitted students using point chart
plot(x = 1,
     type = "n",
     xlim = c(0,80), 
     ylim = c(300,900),
     pch = 16,
     ylab = "Gre Marks",
     main = "Gre marks of admitted students")

points(x = Low,
       pch = 16,
       col ="blue")

points(x = medium,
       pch = 16,
       col = "red")
points(x = high,
       pch = 16,
       col = "black")
legend("bottomright",
       legend = c("High","Medium","Low"),
       col = c('black', 'red','blue'),
       pch = c(16, 16))




