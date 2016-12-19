
My.Location <- ""
setwd(My.Location)
require(caret)
library("tidyverse")

train <- read.csv("myTrain1.csv")
test <- read.csv("myTest1.csv")
unique(train$scheme_management)
train$scheme_management <- as.character(train$scheme_management)
train$scheme_management <- ifelse(is.na(train$scheme_management),"Other",train$scheme_management)
train$scheme_management <- ifelse(train$scheme_management == "","Other",train$scheme_management)
train$scheme_management <- as.factor(train$scheme_management)

test$scheme_management <- as.character(test$scheme_management)
test$scheme_management <- ifelse(is.na(test$scheme_management),"Other",test$scheme_management)
test$scheme_management <- ifelse(test$scheme_management == "","Other",test$scheme_management)
test$scheme_management <- as.factor(test$scheme_management)
table(train$scheme_management)
table(test$scheme_management)

missing.records <- train[!complete.cases(train),]

table(train$public_meeting)
table(train$permit)
table(test$public_meeting)
table(train$permit)
str(train$public_meeting)

train$public_meeting <- as.character(train$public_meeting)
train$public_meeting <- ifelse(train$public_meeting == "","False",train$public_meeting)
train$public_meeting <- as.factor(train$public_meeting)

test$public_meeting <- as.character(test$public_meeting)
test$public_meeting <- ifelse(test$public_meeting == "","False",test$public_meeting)
test$public_meeting <- as.factor(test$public_meeting)

train$permit <- as.character(train$permit)
train$permit <- ifelse(train$permit == "","False",train$permit)
train$permit <- as.factor(train$permit)

test$permit <- as.character(test$permit)
test$permit <- ifelse(test$permit == "","False",test$permit)
test$permit <- as.factor(test$permit)

table(train$permit)
table(test$permit)
table(train$public_meeting)
table(test$public_meeting)

missing.records <- train[!complete.cases(train),]

str(train)

# dummies <- dummyVars(id ~ ., data = train)
# train2 <- as.data.frame(predict(dummies, newdata = train))
# train2 <- cbind(id=train$id, train2)
# test <- as.data.frame(predict(dummies, newdata = test))
# test <- cbind(id=test$id, test)

str(train)

train_label <- read_csv('Training_set_Labels.csv')
train <- train %>%
  left_join(train_label,by="id")

# str(train$status_group)
train$status_group <- as.factor(train$status_group)

# install.packages("flexclust")
library("flexclust")
classification.table <- matrix(ncol=5, nrow=0)
# colnames(classification.table) <- c("K","mtry","ntree","dev.rate","val.rate")
for (k1 in 30:30){
  k1<-30
    cl1 = kcca(cbind(train$latitude, train$longitude), k=k1, kccaFamily("kmeans"))
    # cl1
    pred_train <- predict(cl1)
    # image(cl1)
    # points(cbind(train$latitude, train$longitude), col=pred_train, pch=19, cex=0.3)
    pred_train <- as.data.frame(pred_train)
    pred_train$columnid <- c(1:nrow(pred_train))
    train$columnid <- c(1:nrow(train))
    train <- train %>%
      left_join(pred_train,by="columnid")
    train$kmeans_class <- as.factor(train$pred_train)  
    train <- train[,-(ncol(train)-1)]
    train <- train[,-(ncol(train)-1)]
    
    # Predicting Test clusters 
    pred_test <- predict(cl1, newdata=cbind(test$latitude, test$longitude))
    pred_test <- as.data.frame(pred_test)
    pred_test$columnid <- c(1:nrow(pred_test))
    test$columnid <- c(1:nrow(test))
    test <- test %>%
      left_join(pred_test,by="columnid")
    test$kmeans_class <- as.factor(test$pred_test)  
    test <- test[,-(ncol(test)-1)]
    test <- test[,-(ncol(test)-1)]
    
    # str(train$`funder.Government Of Tanzania`)
    # 
    # names(train) <- gsub(" ", ".", names(train))
    # names(train) <- gsub("/", "_", names(train))
    # names(train) <- gsub("-", "_", names(train))
    # 
    # names(test) <- gsub(" ", ".", names(test))
    # names(test) <- gsub("/", "_", names(test))
    # names(test) <- gsub("-", "_", names(test))
    
    set.seed(123)
    d <- sort(sample(nrow(train), nrow(train)*1))
    dev<-train[d,]
    val<-train[-d,]
    # table(train$status_group)
    
    ### Random Forest
    # library(randomForest)
    # library(matlab)
    # library(e1071)
    # library(caret)
    # library(deepnet)
    # library(gbm)
    # library(aod) # For chi square test
    # 
    # library(ROCR) # For ROC, AUC calculation
    # library(rms)
    # library(dtplyr)
    # library(tidyverse)
    
    for (mtry in 4:4){
      for (ntree in 200:400){
        mtry <- 4
        ntree <- 212
          rf = randomForest(status_group~. , dev[,-c(1,6,7)], mtry=mtry, ntree=ntree)
          rf_predictions = predict(rf,val[,-c(1,6,7)], type="class")
          #rf_predictions
          # table(val$status_group, rf_predictions)
          m <- table(val$status_group, rf_predictions)
          val.rate <- (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
          
          rf_predictions = predict(rf,dev[,-c(1,6,7)], type="class")
          #rf_predictions
          m <- table(dev$status_group, rf_predictions)
          dev.rate<-(m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
          
          
          classification.table <- rbind(classification.table, c(k1,mtry, ntree, val.rate, dev.rate))

          ntree < ntree + 4
    }
}
test <- test[,-(ncol(test))]
train <- train[,-(ncol(train))]
}


rf_predictions = predict(rf,test[,-c(1,6,7)], type="class")
str(rf_predictions)
output_test <- cbind(test$id,as.character(rf_predictions))
colnames(output_test) <- c("id","status_group")
write.csv(output_test, "SubmissionFormat_LaCanyon12.csv", row.names=FALSE)



classification.table <data.frame(classification.table)

colnames(classification.table) <- c("K","mtry","ntree","val.rate","dev.rate")
write.csv(classification.table, "classification.table4.csv", row.names=FALSE)


################### ADA Boost Models ##############################

set.seed(123)
d <- sort(sample(nrow(train), nrow(train)*8))
dev<-train[d,]
val<-train[-d,]

library(rpart)
mfinal <- 25
maxdepth <- 5

rf = randomForest(status_group~. , dev[,-c(1,6,7)], mtry=mtry, ntree=ntree)
?boosting
train.adaboost <- boosting(status_group ~.,data=dev[,-c(1,6,7)],mfinal=mfinal, coeflearn="Zhu",
                           control=rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(train.adaboost,newdata=val[,-c(1,6,7)])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error



################### NeuralNet Models ##############################
library(nnet)

weighted.acc <- function(predictions, actual)
{
  freqs <- as.data.frame(table(actual))
  tmp <- t(mapply(function (p, a) { c(a, p==a) }, predictions, actual, USE.NAMES=FALSE)) # map over both together
  tab <- as.data.frame(table(tmp[,1], tmp[,2])[,2]) # gives rows of [F,T] counts, where each row is a state
  acc.pc <- tab[,1]/freqs[,2]
  return(sum(acc.pc)/length(acc.pc))
}

results <- matrix(ncol=6, nrow=0)
models <- list()

cw1 <- rep(1, 3) # all equal
cw2 <- c(10, 100, 10) # powers of 10
freqs <- as.data.frame(table(dev$status_group))
cw3 <- cbind(freqs[1], apply(freqs, 1, function(s) { length(dev[,34])/as.integer(s[2])})) # 1/counts

class.weights <- rbind(cw1, cw2, cw3[,2])
colnames(class.weights) <- c("functional", "functional needs repair", "non functional")

c <- 3

data.weights <- do.call(rbind, Map(function(s)
{
  class.weights[c,s]
}, dev$status_group))

h<- 20
  ann <- nnet(status_group~., data=dev[,-c(1,6,7)], weights=data.weights[], size=h, decay=5e-1, maxit=200,MaxNWts = 7000)
  
  pred <- predict(ann, dev[,-c(1,6,7)], type="class")
  tacc <- weighted.acc(pred, dev[,34]) # weighted training accuracy
  
  m<-table(dev$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
  
  pred <- predict(ann, val[,-c(1,6,7)], type="class")
  wacc <- weighted.acc(pred, val[,34]) # weighted test accuracy
  
  m<-table(val$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])
  
  results <- rbind(results, c(h, tacc, wacc, c))
  models[[(length(models)+1)]] <- ann

  table(dev$status_group, pred)
  m<-table(dev$status_group, pred)
  (m[1,1]+m[2,2]+m[3,3])/(m[1,1]+m[1,2]+m[1,3]+m[2,2]+m[2,1]+m[2,3]+m[3,1]+m[3,2]+m[3,3])






for (i in 1:3)
{
  
  for (c in 1:length(class.weights[,1]))
  {
    
    data.weights <- do.call(rbind, Map(function(s)
    {
      class.weights[c,s]
    }, dev$status_group))
    
    for (h in 2:30)
    {
      
      ann <- nnet(status_group~., data=dev[,-c(1,6,7)], weights=data.weights[], size=h, decay=5e-4, maxit=200)
      
      pred <- predict(ann, dev[,-c(1,6,7)], type="class")
      tacc <- weighted.acc(pred, dev[,34]) # weighted training accuracy
      pred <- predict(ann, dev[,-c(1,6,7)], type="class")
      wacc <- weighted.acc(pred, val[,34]) # weighted test accuracy
      
      results <- rbind(results, c(h, tacc, wacc, c))
      models[[(length(models)+1)]] <- ann
    }
  }
}

seedsANN =nnet(status_group~., dev[,-c(1,6,7)],size=4, decay=5e-4, maxit=200)
pred <- predict(seedsANN, val[,-c(1,6,7)], type="class")
table(val$status_group, pred)
m<-table(val$status_group, pred)
(m[1,1]+m[3,2])/(m[1,1]+m[1,2]+m[2,2]+m[2,1]+m[3,1]+m[3,2])
tacc <- weighted.acc(pred, val[,34])

