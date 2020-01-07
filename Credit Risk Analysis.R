
#Loading the libraries
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ROCR)
library(dplyr)


#Loading the dataset
GermanCredit_assgt1_F18 <- read.csv("/data.csv")
View(GermanCredit_assgt1_F18)

#Viewing the dataset 
mdData<-GermanCredit_assgt1_F18
View(mdData)

#
summary(mdData)
attributes(mdData)
str(mdData)

#Changing to Factor
cols <- c("RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES","NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING", "MALE_DIV", "MALE_SINGLE", "MALE_MAR_or_WID", "CO-APPLICANT","GUARANTOR","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","CHK_ACCT", "HISTORY", "SAV_ACCT", "EMPLOYMENT","PRESENT_RESIDENT","JOB")
mdData[cols] <- lapply(mdData[cols], factor)
sapply(mdData, class)

#Changing to Numeric values
colmn <- c("NEW_CAR", "USED_CAR", "FURNITURE", "RADIO/TV", "EDUCATION", "RETRAINING")
mdData[colmn] <- lapply(mdData[colmn], as.numeric)
sapply(mdData, class)


#Developing Decision Tree Model
#Model 1
rpModel1=rpart(RESPONSE ~ ., data=mdData, method="class")
print(rpModel1)
rpart.plot::prp(rpModel1, type=2, extra=1)
summary(rpModel1)
plotcp(rpModel1)
pred1=predict(rpModel1, mdData, type='class')
table(pred = pred1, true = mdData$RESPONSE)
confusionMatrix(pred1,mdData$RESPONSE) #Confisuion Matrix for Model 1
mean(pred1==mdData$RESPONSE)



#Model2 - Based on Gini
rpmodel2 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'))
rpart.plot::prp(rpmodel2, type=2, extra=1)
summary(rpmodel2)
plotcp(rpmodel2)
pred2=predict(rpmodel2, mdData, type='class')
table(pred = pred2, true = mdData$RESPONSE)
confusionMatrix(pred2,mdData$RESPONSE) #Confisuion Matrix for Model 2
mean(pred2==mdData$RESPONSE)



#Model 4 - Based on information
rpmodel3 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='information'))
rpart.plot::prp(rpmodel3, type=2, extra=1)
summary(rpmodel3)
plotcp(rpmodel3)
pred3=predict(rpmodel3, mdData, type='class')
table(pred = pred3, true = mdData$RESPONSE)
confusionMatrix(pred2,mdData$RESPONSE) #Confisuion Matrix for Model 4
mean(pred3==mdData$RESPONSE)



#Model 5 - Based on Gini and MinSplit and MaxDepth
rpmodel5 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(minsplit = 30,
                                                                                                             maxdepth = 5,maxsurrogate = 0,xval = 2, minbucket = 10))
rpart.plot::prp(rpmodel5, type=2, extra=1)
summary(rpmodel5)
plotcp(rpmodel5)
pred5=predict(rpmodel5, mdData, type='class')
table(pred = pred5, true = mdData$RESPONSE)
confusionMatrix(pred5,mdData$RESPONSE) #Confisuion Matrix for Model 5
mean(pred5==mdData$RESPONSE)


#Model 6 - Based on Gini and MaxSplit

rpmodel6 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(maxdepth = 25))
                                                                                                               
rpart.plot::prp(rpmodel6, type=2, extra=1)
summary(rpmodel6)
plotcp(rpmodel6)
pred6=predict(rpmodel6, mdData, type='class')
table(pred = pred6, true = mdData$RESPONSE)
confusionMatrix(pred6,mdData$RESPONSE) #Confisuion Matrix for Model 6
mean(pred6==mdData$RESPONSE)



#Model 7 - Based on Gini and MinBucket

rpmodel6 = rpart(RESPONSE ~ ., data=mdData, method="class",parms = list(split ='gini'),control = rpart.control(  minbucket = 10))
                                                                                                              
rpart.plot::prp(rpmodel6, type=2, extra=1)
summary(rpmodel6)
plotcp(rpmodel6)
pred6=predict(rpmodel6, mdData, type='class')
table(pred = pred6, true = mdData$RESPONSE)
confusionMatrix(pred6,mdData$RESPONSE) #Confisuion Matrix for Model 7
mean(pred6==mdData$RESPONSE)

install.packages('ROCR')

#score test data set
mdData$score<-predict(rpmodel6,type='prob',mdData)
pred<-prediction(mdData$score[,2],mdData$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="red")

lift1_1<-performance(pred,"lift","rpp")
plot(lift1_1,main="Lift Curve",colorize=F)


#Finding Best Tree
library(rpart)

set.seed(1)
newdata <- GermanCredit_assgt1_F18
str(newdata)
cols <- c("RESPONSE","FOREIGN","TELEPHONE","JOB","OWN_RES","RENT","OTHER_INSTALL","PROP_UNKN_NONE","REAL_ESTATE","PRESENT_RESIDENT","GUARANTOR","CO.APPLICANT","MALE_MAR_or_WID","MALE_SINGLE","MALE_DIV","EMPLOYMENT","SAV_ACCT","RETRAINING","EDUCATION","RADIO.TV","FURNITURE","USED_CAR","NEW_CAR","HISTORY","CHK_ACCT")
newdata[,cols] <- lapply(newdata[,cols], factor)
str(newdata)
trnindex <- sample(1:nrow(mdData), size=round(nrow(mdData)*0.5), replace=F)
TrnData <- mdData[trnindex,]
TstData <- mdData[-trnindex,]
dim(TrnData)
dim(TstData)

summary(TstData)


#develop a tree on the training data
library(rpart)
rpmodelfinal = rpart(RESPONSE ~ ., data=TrnData, method="class",parms = list(split ='gini'),control = rpart.control(  minsplit = 25 ))
pred6=predict(rpmodel6, mdData, type='class')

predTst <- predict(rpart.fit, TstData, type = "class")


#Confusion table
table(pred = predTst, true = TstData$RESPONSE)
#Confisuion Matrix
install.packages("caret")


PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel6,mdTst, type="prob")[,'1'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, mdTst$RESPONSE)


prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`mdTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
plot(prLifts$cumProfits)



#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
```


