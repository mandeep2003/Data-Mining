bdata<-read.csv("D://Old Laptop/1 PG in Business Analytics XLRI/Study Material/Data Mining/Assignment/bankdataPWork.csv", header = TRUE)
head(bdata)
class(bdata)
is.null(bdata) # No Null values OR No missing data.

#Visualize the data
plot(bdata$y,bdata$age, ylab="Age")
plot(bdata$y,bdata$job, ylab="Job")
plot(bdata$y,bdata$loan, ylab="loan")
qqplot(bdata$y,bdata$job)
qqplot(bdata$y,bdata$age)
plot(bdata$y,bdata$education,ylab="Education" )


set.seed(123)
summary(bdata) #To know the data.
nrow(subset(bdata,y=="yes")) #only 4640 yes in entire data set. data is not balanced. 
library(caTools) #Library for sampling statistics
smpl<-sample.split(bdata$y, SplitRatio = .7)
head(smpl)
bdata_trng<-subset(bdata,smpl==TRUE) #prepare training data
bdata_tst<-subset(bdata,smpl==FALSE) #prepare test data
nrow(bdata_trng) #28832
nrow(bdata_tst) #12356
bdata_trng_y<-subset(bdata_trng,y=="yes")
bdata_trng_n<-subset(bdata_trng,y=="no")
nrow(bdata_trng_y) # only 3248
nrow(bdata_trng_n) # 25584

#Over sampling because under sampling will lose the information.
o_smpl<-sample(1:nrow(bdata_trng_y),nrow(bdata_trng_n), replace = TRUE)
bdata_trng_y_o<-bdata_trng_y[o_smpl,]
nrow(bdata_trng_y_o) #25584 same as "no"
bdata_trng_final<-rbind(bdata_trng_y_o,bdata_trng_n)
nrow(bdata_trng_final)
#Logistics Regression Model
library(car)
library(ROCR)
model_LR<-glm(y~., data = bdata_trng_final, family = "binomial")
summary(model_LR)
?vif
vif(model_LR) # throws error "there are aliased coefficients in the model"
# Need to remove the linearly dependent variable.
#below find the linearly dependent variable.
ld_vars <- attributes(alias(model_LR)$Complete)$dimnames[[1]]
#Re-Build the model
model_LR<-glm(y~.-loan, data = bdata_trng_final, family = "binomial")
vif(model_LR)
#Refine model by removing predictor variable vif > 5.
model_LR<-glm(y~age+marital+education+default+housing+contact+day_of_week+duration+campaign, data = bdata_trng_final, family = "binomial")
vif(model_LR)

#Determine accuracy of the model with test data.
pred_y<-predict(model_LR,bdata_tst, type="response")
pred<-prediction(pred_y,bdata_tst$y)
rocc<-performance(pred,"tpr","fpr")
plot(rocc)
aucrp<-performance(pred,"auc")
aucrp

#Decision Tree Classification
library(rpart)
model_DT<-rpart(y~.,data=bdata_trng_final, method="class")
pred_DT<-predict(model_DT,bdata_tst, type = "prob")
head(pred_DT)
prsc_DT<-pred_DT[,2]

pred<-prediction(prsc_DT,bdata_tst$y)
rocc<-performance(pred,"tpr","fpr")
plot(rocc)
aucrp<-performance(pred,"auc")
aucrp


#Bayes algorithms.
library(e1071)
model_BA<-naiveBayes(y~.,data = bdata_trng_final)
summary(model_BA)
pred_BA<-predict(model_BA,bdata_tst,type = "raw")
prsc_BA<-pred_BA[,2]
pred<-prediction(prsc_BA,bdata_tst$y)
rocc<-performance(pred,"tpr","fpr")
plot(rocc)
aucrp<-performance(pred,"auc")
aucrp
