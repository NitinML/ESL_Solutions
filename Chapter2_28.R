################   Solution for Chapter 2 - Problem 2.8   ######################

###############  Implementing kNN    #######################


# Setting working directory

setwd("C:/Users/Nitin/Desktop/ML_AI_Work/ESL")

# Getting Training and Test Dataset for Digits 2 & 3

train2 <- read.table('train.2',sep = ",")
train3 <- read.table('train.3',sep=',')
test <- read.table('zip.test', sep = " ")
test2 <- test[which(test$V1 == '2'),]
test3 <- test[which(test$V1 == '3'),]

#Getting information about the columns in training datasets
head(train2,n=5)

# Adding columns for 2 and 3 and combining two datasets

train2$Response <- 2
train3$Response <- 3
train <- rbind(train2,train3)
test <- rbind(test2,test3)

train_response <- train[,ncol(train)]
train_k <- train[,-ncol(train)]

train <- cbind(train_response,train_k)

kNN(1,train,test)
kNN(3,train,test)
