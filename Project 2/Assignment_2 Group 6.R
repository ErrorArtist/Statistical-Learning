rm(list=ls())

library("easypackages")
libraries("MASS","ISLR2","leaps","glmnet","pls","writexl","randomForest")

data1<-read.csv("dataset1.csv")
data2<-read.csv("dataset2.csv")
data3<-read.csv("dataset3.csv")
data4<-read.csv("dataset4.csv")
data5<-read.csv("dataset5.csv")
dim(data1)
dim(data2)
dim(data3)
dim(data4)
dim(data5)

#1. Logistic Regression
#2. Quadratic Discriminant Analysis
#3. A random forest with 500 trees using variables and the seed number 2023 in R

###Data1 lr
#The features are independent and normally distributed within 
#each target class, with variances either being 1 or 10. 
#Are the (population) covariance matrices the same across the target classes?
fit1 <- glm(target~features_1+features_2+features_3+features_4+features_5,
            data = data1 , family = binomial)
p1<-predict(fit1,type="response")
pp1<-log(p1/(1-p1))


###Data2 lr
#The log odds ratio is a linear function of the features. 
#Are the features normally distributed?
fit2 <- glm(target~features_1+features_2+features_3+features_4+features_5,
            data = data2 , family = binomial)
p2<-predict(fit2,type="response")
pp2<-log(p2/(1-p2))


###Data3 rf
#The log odds ratio is a highly nonlinear-not quadratic- function of the features


data3$target = as.factor(data3$target)
set.seed(2023)
forest.fit = randomForest(target~.,data=data3, ntree = 500,  mtry = sqrt(ncol(data3) - 1))
forest.pred = predict(forest.fit, data3 ,type="prob")[,"1"]
forest.pred_0 = predict(forest.fit, data3 ,type="prob")[,"0"]
result3 = log(forest.pred/forest.pred_0)


###Data4 qda
#The sample covariance matrices are close to the population covariance matrices 
#in each target class. The features are normally distributed within each target 
#class. 
#Are the (population) covariance matrices the same across the target classes?
fit4<-qda(target~features_1+features_2+features_3,data = data4)
p4<-predict(fit4)
pp4<-log(p4$posterior[,2]/p4$posterior[,1])







### Data5 lr
#The population covariance matrices among features are not equal but are very 
#close across target classes. The features are normally distributed within each
#target class. 
fit5 <- glm(target~.,data = data5 , family = binomial)
p5<-predict(fit5,type="response")
pp5<-log(p5/(1-p5))

answers<-read.csv("Answer_Sheet.csv")
answers$Data.Set.1 = pp1
answers$Data.Set.2 = pp2
answers$Data.Set.3 = result3
answers$Data.Set.4 = pp4
answers$Data.Set.5 = pp5
write.csv(answers, "Answer_Sheet.csv", row.names = FALSE)


