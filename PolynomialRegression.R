data(package = .packages(all.available = TRUE))
library('caret')
library('psych')
data<-GermanCredit
View(data)
describe(data)
summary(data)

#=====================================================================================
#                 Polynomial Regression for sample size 100
#=====================================================================================
set.seed(0)
random = sample(1:nrow(data),100)
train = data[random, ]

set.seed(1)
random1= sample(1:nrow(data),100)
test=data[random1, ]
#=======================Polynomial regression for degree 1==================

model1<-lm(Amount~Age,train)
model1

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",1,".jpeg"))
plot(train$Age,train$Amount, pch=19, cex=0.2,xlab="Age",ylab="Amount",ylim=c(0,7000), main="Polynomial Regression (Degree 1)")
lines(sort(train$Age), fitted(model1)[order(train$Age)], col=1, type='l',pch=19) 
dev.off()

#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model1$residuals^2))/100)
pred = predict(model1, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)

#=======================Polynomial regression for degree 2==================

model2 <- lm(Amount ~ Age + I(Age^2), train)
model2

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",2,".jpeg"))
plot(train$Age,train$Amount, pch=19, cex=0.2,ylim=c(0,7000), xlab="Age",ylab="Amount",main="Polynomial Regression (Degree 2)")
lines(sort(train$Age), fitted(model2)[order(train$Age)], col=2, type='l', pch=19) 
dev.off()
#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model2$residuals^2))/100)
pred = predict(model2, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)

#======================Polynomial regression for Degree 7====================

model3 <- lm(Amount ~ Age + I(Age^2)+ I(Age^3) + I(Age^4)+ I(Age^5) + I(Age^6)+ I(Age^7), train)
model3

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",3,".jpeg"))
plot(train$Age,train$Amount, pch=19, cex=0.2, ylim=c(0,7000), xlab="Age",ylab="Amount",main="Polynomial Regression (Degree 7)")
lines(sort(train$Age), fitted(model3)[order(train$Age)], col=3, type='l', pch=19) 
dev.off()
#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model3$residuals^2))/100)
pred = predict(model3, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)

#======================Polynomial regression for Degree 8====================

model4 <- lm(Amount ~ Age + I(Age^2)+ I(Age^3) + I(Age^4)+ I(Age^5) + I(Age^6)+ I(Age^7)+ I(Age^8), train)
model4

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",4,".jpeg"))
plot(train$Age,train$Amount, pch=19, ylim=c(0,7000),cex=0.2, xlab="Age",ylab="Amount",main="Polynomial Regression (Degree 8)")
lines(sort(train$Age), fitted(model4)[order(train$Age)], col=4, type='l', pch=19) 
dev.off()
#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model4$residuals^2))/100)
pred = predict(model4, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)

#======================Polynomial regression for Degree 9====================

model5 <- lm(Amount ~ Age + I(Age^2)+ I(Age^3) + I(Age^4)+ I(Age^5) + I(Age^6)+ I(Age^7)+ I(Age^8) +I(Age^9), train)
model5

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",5,".jpeg"))
plot(train$Age,train$Amount, pch=19,ylim=c(0,7000), cex=0.2, xlab="Age",ylab="Amount",main="Polynomial Regression (Degree 9)")
lines(sort(train$Age), fitted(model5)[order(train$Age)], col=5, type='l', pch=19) 
dev.off()
#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model5$residuals^2))/100)
pred = predict(model5, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)


#======================Polynomial regression for Degree 10====================

model6 <- lm(Amount ~ Age + I(Age^2)+ I(Age^3) + I(Age^4)+ I(Age^5) + I(Age^6)+ I(Age^7)+ I(Age^8) +I(Age^9)+I(Age^10), train)
model6

#PLOTTING THE MODEL OVER THE DATA
jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/PR_Degree",6,".jpeg"))
plot(train$Age,train$Amount, pch=19, cex=0.2,ylim=c(0,7000), xlab="Age",ylab="Amount",main="Polynomial Regression (Degree 10)")
lines(sort(train$Age), fitted(model6)[order(train$Age)], col=6, type='l', pch=19) 
dev.off()
#TRAIN AND TEST ACCURACY
Train_RMSE=sqrt((sum(model6$residuals^2))/100)
pred = predict(model6, newdata=test)
Test_RMSE=sqrt((sum((pred-test$Amount)^2))/100)

#================================Polynomial Regression for different sample size==========================================
l=c(20,50,100,200,350,600)
Test_Error=c()
Train_Error=c()
for (i in l){
  random=sample(1:nrow(data),i)
  train=data[random,]
  test=data[-random,]
   
  model=lm(Amount~Age+I(Age^2)+I(Age^3)+I(Age^4)+I(Age^5)+I(Age^6)+I(Age^7),train)
   jpeg(filename=paste("C:/Users/Nidhi/Desktop/ML-2/PolynomialRegression",i,".jpeg"))
   plot(train$Age,train$Amount,xlab="Age", ylab="Amount", main=paste("Polynomial Regression (Degree 7) for",i, "Samples"),pch=19,cex=0.5)
   lines(sort(train$Age),fitted(model)[order(train$Age)],col='red',type='l')
   dev.off()
  Train_Error=sum(model$residuals^2)
  pred = predict(model, newdata=test)
  Test_Error=append(Test_Error,sum((pred-test$Amount)^2))
  train=c()
  test=c()
}
jpeg(filename="C:/Users/Nidhi/Desktop/ML-2/TestError.jpeg")
plot(l,Test_Error,pch=19,cex=0.5,xlab="Sample size",ylab="Test Error",ylim=c(10^9,3*10^10), main="Test Error v/s Sample Size" )
lines(l,Test_Error,col="red",type="l")
dev.off()


#===========================RMSE for different Complexity===============================================
RMSE_Train=c()
RMSE_Test=c()
random=c()
train=c()
test=c()
n=0
i=1
l=c()
s=c(1,2,3,4)
complexity=c(1,2,7,8,9,10)
m=c(20,100)
for (j in m){
  for (i in s){
    if(j==100){
      i=i+4
    }
l=sample(1:nrow(data),j)
 
  train=data[l,c("Age","Amount")]
  test=data[-l,]
  
  model1<-lm(Amount~Age,train)
  model2<-lm(Amount~Age+I(Age^2),train)
  model3<-lm(Amount~Age+I(Age^2)+I(Age^3)+I(Age^4)++I(Age^5)+I(Age^6)+(Age^7),train)
  model4<-lm(Amount~Age+I(Age^2)+I(Age^3)+I(Age^4)++I(Age^5)+I(Age^6)+I(Age^7)+I(Age^8),train)
  model5<-lm(Amount~Age+I(Age^2)+I(Age^3)+I(Age^4)++I(Age^5)+I(Age^6)+I(Age^7)+I(Age^8)+I(Age^9),train)
  model6<-lm(Amount~Age+I(Age^2)+I(Age^3)+I(Age^4)++I(Age^5)+I(Age^6)+I(Age^7)+I(Age^8)+I(Age^9)+I(Age^10),train)
  
  jpeg(filename = paste("C:/Users/Nidhi/Desktop/ML-2/Samplesize",i,".jpeg"))
  
  plot(train$Age,train$Amount,xlab="Age",ylab="Amount", xlim=c(20,70), ylim=c(0,10000),main=paste("Polynomial Regression for different complexity"), pch=19, cex=0.5)
  lines(sort(train$Age),fitted(model1)[order(train$Age)],col=1,type="l")
  lines(sort(train$Age),fitted(model2)[order(train$Age)],col=2,type="l")
  lines(sort(train$Age),fitted(model3)[order(train$Age)],col=3,type="l")
  lines(sort(train$Age),fitted(model4)[order(train$Age)],col=4,type="l")
  lines(sort(train$Age),fitted(model5)[order(train$Age)],col=5,type="l")
  lines(sort(train$Age),fitted(model6)[order(train$Age)],col=6,type="l")
  legend(60, 10000, legend=c("Order1", "Order2","Order7","Order8","Order9","Order10"),
    col=c(1,2,3,4,5,6), lty=1:2, cex=0.9)
  dev.off()
  
  RMSE_Train=append(RMSE_Train,sqrt((sum(model1$residuals^2))/j))
  RMSE_Train=append(RMSE_Train,sqrt((sum(model2$residuals^2))/j))
  RMSE_Train=append(RMSE_Train,sqrt((sum(model3$residuals^2))/j))
  RMSE_Train=append(RMSE_Train,sqrt((sum(model4$residuals^2))/j))
  RMSE_Train=append(RMSE_Train,sqrt((sum(model5$residuals^2))/j))
  RMSE_Train=append(RMSE_Train,sqrt((sum(model6$residuals^2))/j))
  
  #plot(Train_Error,)
  pred = predict(model1, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  pred = predict(model2, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  pred = predict(model3, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  pred = predict(model4, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  pred = predict(model5, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  pred = predict(model6, newdata=test)
  RMSE_Test=append(RMSE_Test,sqrt((sum((pred-test$Amount)^2))/(1000-j)))
  
  jpeg(filename=paste("C:/Users/Nidhi/Desktop/ML-2/Error",i+8,".jpeg"))
  plot(range(0:10),range(0:10^4),xlab="Complexity", ylab="RMSE", main=paste("RMSE v/s Complexity for Sample size",j),pch=19,cex=0.5)
  lines(complexity,RMSE_Test,col=2,type='l',pch=19)
  lines(complexity,RMSE_Train,col=1,type='l',pch=19)
  legend(7, 9000, legend=c("RMSE(Train)", "RMSE(Test)"),
         col=c(1,2), lty=1:2, cex=0.8)
  dev.off()
  
  random=c()
  train=c()
  test=c()
  
l=c()
RMSE_Train=c()
RMSE_Test=c()
  }
}
