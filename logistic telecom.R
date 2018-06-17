## setting working directory
setwd("C:\\Users\\Puneet\\Downloads")
data= read.csv("telecomcustomer.csv")

## structure of dataset
str(data)

## to check target variable is balanced or not
prop.table(table(data$Churn))

## check levels in factor variables
table(data$MultipleLines)
## conert all sentence that start with no to only no like
## no phone service to no
data$MultipleLines=ifelse(grepl("No",data$MultipleLines,fixed=TRUE)==TRUE,"No","Yes")
table(data$MultipleLines)

table(data$InternetService)
## here three levels are there but fiber optic has space so remove that
data$InternetService=gsub("Fiber optic","fiberoptic",data$InternetService)
table(data$InternetService)

table(data$OnlineSecurity)
data$OnlineSecurity=ifelse(grepl("No",data$OnlineSecurity,fixed=TRUE)==TRUE,"No","Yes")
table(data$OnlineSecurity)

table(data$OnlineBackup)
data$OnlineBackup=ifelse(grepl("No",data$OnlineBackup,fixed=TRUE)==TRUE,"No","Yes")
table(data$OnlineBackup)

table(data$DeviceProtection)
data$DeviceProtection=ifelse(grepl("No",data$DeviceProtection,fixed=TRUE)==TRUE,"No","Yes")
table(data$DeviceProtection)

table(data$TechSupport)
data$TechSupport=ifelse(grepl("No",data$TechSupport,fixed=TRUE)==TRUE,"No","Yes")
table(data$TechSupport)

table(data$StreamingTV)
data$StreamingTV=ifelse(grepl("No",data$StreamingTV,fixed=TRUE)==TRUE,"No","Yes")
table(data$StreamingTV)

table(data$StreamingMovies)
data$StreamingMovies=ifelse(grepl("No",data$StreamingMovies,fixed=TRUE)==TRUE,"No","Yes")
table(data$StreamingMovies)

table(data$PaymentMethod)
data$PaymentMethod=gsub(" ","",data$PaymentMethod)
table(data$PaymentMethod)

table(data$Contract)
data$Contract=gsub(" ","",data$Contract)
table(data$Contract)


data$customerID=NULL

## covert character variables to factor variables
for (i in 1:ncol(data)){
  if (class(data[,i])=="Character"){
    data[,i]=as.factor(data[,i])
  }
}
table(data$SeniorCitizen)
data$SeniorCitizen=ifelse(data$SeniorCitizen==1,"yes","no")
table(data$SeniorCitizen)

## checking missing values
sum(is.na(data))
## first checking for numeric values
sum(is.na(data$MonthlyCharges))
sum(is.na(data$TotalCharges))
## all 11 missing values is from totalcharges only
## fill this with mean
data$TotalCharges[is.na(data$TotalCharges)]=mean(data$TotalCharges,na.rm=TRUE)
sum(is.na(data$TotalCharges))                 

## finding corelation 
names(data)
cor(data[,c(5,18,19)])
## tenure and totalcharges has high corelation so take any one
data$tenure=NULL
## EDA for factor variables 
## as i/p and o/p are factor so use bar chart
library(ggplot2)
ggplot(data,aes(gender,fill=Churn))+geom_bar()
## so it is not significant we can remove that
data$gender=NULL

ggplot(data,aes(SeniorCitizen,fill=Churn))+geom_bar()
## significant

ggplot(data,aes(Partner,fill=Churn))+geom_bar()
## significant

ggplot(data,aes(Dependents,fill=Churn))+geom_bar()
##significant

ggplot(data,aes(PhoneService,fill=Churn))+geom_bar()
##significant

ggplot(data,aes(MultipleLines,fill=Churn))+geom_bar()
##significant

ggplot(data,aes(InternetService,fill=Churn))+geom_bar()
##significant

ggplot(data,aes(OnlineSecurity,fill=Churn))+geom_bar()
## significant

ggplot(data,aes(OnlineBackup,fill=Churn))+geom_bar()
## significant

ggplot(data,aes(DeviceProtection,fill=Churn))+geom_bar()

ggplot(data,aes(TechSupport,fill=Churn))+geom_bar()

ggplot(data,aes(StreamingTV,fill=Churn))+geom_bar()

ggplot(data,aes(StreamingMovies,fill=Churn))+geom_bar()

ggplot(data,aes(Contract,fill=Churn))+geom_bar()

ggplot(data,aes(PaperlessBilling,fill=Churn))+geom_bar()

ggplot(data,aes(PaymentMethod,fill=Churn))+geom_bar()
## above all are significant

## split train and test
idx=sample(nrow(data), nrow(data)*0.8)
set.seed(988)
train=data[idx, ]
test=data[-idx, ]

## train a logistic regression model
model1 = glm(Churn~.,data=train,family = binomial)
summary(model1)

model2=glm(Churn~ TotalCharges+PaymentMethod+PaperlessBilling+Contract+SeniorCitizen, data=train, family=binomial)
summary(model2)

install.packages("ROCR")
library(ROCR)

train$pred= predict(model2,newdata=train, type="response")
pred= prediction(train$pred, train$Churn)
perf= performance(pred,"tpr","fpr")

plot(perf)

test$pred= predict(model2,newdata=test, type="response")
pred1= prediction(test$pred, test$Churn)
perf1= performance(pred,"tpr","fpr")

plot(perf1)

## test matrix
test$class= ifelse(test$pred>=0.5,"yes","no")
table(test$Churn,test$class)

precision=152/(111+152)
152/(111+152)

recall= 152/(152+198)
152/(152+198)
