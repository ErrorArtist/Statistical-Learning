## Simon Mutus, Aat Langevoort, Bowen Ma

library("easypackages")
libraries("MASS","ISLR2","leaps","glmnet","pls","writexl")
bestpara <- read.csv("bestpara.csv")
ans <- read.csv("Answer_Sheet.csv")
sum(is.na(ans))

data1<-read.csv("dataset1.csv")
data2<-read.csv("dataset2.csv")
data3<-read.csv("dataset3.csv")
data4<-read.csv("dataset4.csv")
data5<-read.csv("dataset5.csv")

## Data1 Ridge
target1<-data1$target
x<-model.matrix(target~.-1,data=data1)
y<-target1
ridge.mod<-glmnet(x,y,lambda = bestpara$dataset1.csv[2],alpha=0)
predict1<-predict(ridge.mod,lambda = bestpara$dataset1.csv[2],alpha=0,newx = x)
predict1

## Data2 Lasso
target2<-data2$target
x<-model.matrix(target~.-1,data=data2)
y<-target2
lasso.mod<-glmnet(x,y,lambda = bestpara$dataset2.csv[3],alpha=1)
predict2<-predict(lasso.mod,lambda = bestpara$dataset2.csv[3],alpha=0,newx = x)
predict2

## Data3 Ridge
target3<-data3$target
x<-model.matrix(target~.-1,data=data3)
y<-target3
ridge.mod<-glmnet(x,y,lambda = bestpara$dataset3.csv[2],alpha=0)
predict3<-predict(ridge.mod,lambda = bestpara$dataset3.csv[2],alpha=0,newx = x)

## Data4 fwd
reg.fwd <- regsubsets (target~.-1, data = data4,method = "forward")
predict.regsubsets = function(object, newdata, id, ...) {
  form  <-  as.formula(~.)
  mat  <-  model.matrix(form, newdata)
  coeficients  <-  coef(object, id)
  xvars  <-  names(coeficients)
  mat[, xvars] %*% coeficients
}
predict4<-predict.regsubsets(reg.fwd,data4[,-1],6)
predict4

## Data5 pcr
x<-model.matrix(target~.-1,data=data5)
pcr.fit<-pcr(target~.-1, data=data5,scale=TRUE)
predict5<-predict(pcr.fit,data5,ncomp=5)
predict5

## Writing the csv file
answers <- read.csv("Answer_Sheet.csv")
answers$Data.Set.1 = predict1
answers$Data.Set.2 = predict2
answers$Data.Set.3 = predict3
answers$Data.Set.4 = predict4
answers$Data.Set.5 = predict5
write.csv(answers, "Answer_Sheet.csv", row.names = FALSE)

