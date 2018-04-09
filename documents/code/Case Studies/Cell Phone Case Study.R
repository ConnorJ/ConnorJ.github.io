library(reshape)
library(ggplot2)
library(randomForest)
library(dplyr)
library(ade4)
library(e1071)
library(nnet)
library(class)
library(pROC)
#### Load data
phone_full <- read.csv("cell_phone_data.csv", stringsAsFactors=TRUE)
phone <- na.roughfix(phone_full[,-c(1:3,5:6)])

set.seed(1)
size <- .66
sample <- sample(1:nrow(phone), nrow(phone)*size)
train <- phone[sample,]
test <- phone[-sample,]

model <- randomForest(data=train, Status~.)
prediction_test <- predict(model, test)
table(test$Status, prediction_test)
auc(as.numeric(test$Status), as.numeric(prediction_test))

training_set_size <- seq(.10,1.00, by=.10)
info_phone <- data.frame(algorithm=factor(), time=numeric(), accuracy_test=numeric(), accuracy_train=numeric(), accuracy_full=numeric(),train=numeric())
for(size in training_set_size){
  
  for(id in 1:10){
    #
    sample <- sample(1:nrow(phone), nrow(phone)*size)
    train <- phone[sample,]
    test <- phone[-sample,]
    #
    time1 <- Sys.time()
    model <- randomForest(data=train, Status~.)
    prediction_test <- predict(model, test)
    test_time = Sys.time() - time1
    prediction_train <- predict(model, train)
    prediction_full <- predict(model, phone)
    new_row <- data.frame(algorithm = "Random Forest", time = test_time, 
                          accuracy_test = (table(test$Status,prediction_test)[1] + table(test$Status,prediction_test)[4])/nrow(test), 
                          accuracy_train = (table(train$Status,prediction_train)[1] + table(train$Status,prediction_train)[4])/nrow(train), 
                          accuracy_full = (table(phone$Status,prediction_full)[1] + table(phone$Status,prediction_full)[4])/nrow(phone), 
                          train = size)
    info_phone <- rbind(info_phone, new_row)
  }
}

################## SVM ############################

### Clean data
###################################################
#function for normalizing numerical variable 
###################################################
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

###################################################
#for/if loop to transform factor veriables and normalize numeric/integer veriables
###################################################
fc_list <- NULL
fc_list_n <- NULL
n_list <- NULL
i_list <- NULL

for (i in 2:dim(phone)[2]){
  a <- class(phone[,i])
  
  if (a=="factor"){
    
    fc_name <- colnames(phone)[i]
    fc<- acm.disjonctif(data.frame(phone[,i]))
    phone <- cbind(phone,fc)
    fc_list_n <- c(fc_list_n, i)
    fc_list <- c (fc_list, fc_name)
    
  }else {
    
    phone[,i] <- as.numeric(normalize (phone[,i]))
    n_list <- c (n_list, colnames(phone)[i])
    
  }
}

########drop original factor veriables##################
phone <- phone[-c(fc_list_n)]

#### Fit SVM
set.seed(1)
size <- .66
sample <- sample(1:nrow(phone), nrow(phone)*size)
train <- phone[sample,]
test <- phone[-sample,]
#
model <- svm(data=train, Status~.)
prediction_test <- predict(model, test)
table(test$Status, prediction_test)
auc(as.numeric(test$Status), as.numeric(prediction_test))

for(size in training_set_size){
  
  for(id in 1:10){
    #
    sample <- sample(1:nrow(phone), nrow(phone)*size)
    train <- phone[sample,]
    test <- phone[-sample,]
    #
    time1 <- Sys.time()
    model <- svm(data=train, Status~.)
    prediction_test <- predict(model, test)
    test_time = Sys.time() - time1
    prediction_train <- predict(model, train)
    prediction_full <- predict(model, phone)
    new_row <- data.frame(algorithm = "Support Vector Machines", time = test_time, 
                          accuracy_test = (table(test$Status,prediction_test)[1] + table(test$Status,prediction_test)[4])/nrow(test), 
                          accuracy_train = (table(train$Status,prediction_train)[1] + table(train$Status,prediction_train)[4])/nrow(train), 
                          accuracy_full = (table(phone$Status,prediction_full)[1] + table(phone$Status,prediction_full)[4])/nrow(phone), 
                          train = size)
    info_phone <- rbind(info_phone, new_row)
  }
}

################### neural network ###################
set.seed(1)
size <- .66
sample <- sample(1:nrow(phone), nrow(phone)*size)
train <- phone[sample,]
test <- phone[-sample,]
#
model <- nnet(data=train, Status~.,size=10)
prediction_test <- predict(model, test, type="class")
table(test$Status, prediction_test)
auc(as.numeric(test$Status), as.numeric(factor(prediction_test)))

for(size in training_set_size){
  
  for(id in 1:10){
    #
    sample <- sample(1:nrow(phone), nrow(phone)*size)
    train <- phone[sample,]
    test <- phone[-sample,]
    #
    time1 <- Sys.time()
    model <- nnet(data=train, Status~.,size=10)
    prediction_test <- predict(model, test, type="class")
    test_time = Sys.time() - time1
    prediction_train <- predict(model, train, type="class")
    prediction_full <- predict(model, phone, type="class")
    new_row <- data.frame(algorithm = "Neural Network", time = test_time, 
                          accuracy_test = (table(test$Status,prediction_test)[1] + table(test$Status,prediction_test)[4])/nrow(test), 
                          accuracy_train = (table(train$Status,prediction_train)[1] + table(train$Status,prediction_train)[4])/nrow(train), 
                          accuracy_full = (table(phone$Status,prediction_full)[1] + table(phone$Status,prediction_full)[4])/nrow(phone), 
                          train = size)
    info_phone <- rbind(info_phone, new_row)
  }
}



################# KKN #################
set.seed(1)
size <- .66
sample <- sample(1:nrow(phone), nrow(phone)*size)
train <- phone[sample,]
test <- phone[-sample,]
# 
knn_train_no_status <- train[,-1]
knn_test_no_status <- test[,-1]
prediction_test <- knn(train = knn_train_no_status, test = knn_test_no_status, cl=as.factor(train$Status), k = 5)
table(test$Status, prediction_test)
auc(as.numeric(test$Status), as.numeric(factor(prediction_test)))s

for(size in training_set_size){
  
  for(id in 1:10){
    
    sample <- sample(1:nrow(phone), nrow(phone)*size)
    train <- phone[sample,]
    test <- phone[-sample,]
    #
    knn_train_no_status <- train[,-1]
    knn_test_no_status <- test[,-1]
    knn_full_no_status <- phone[,-1]
    
    time1 <- Sys.time()
    prediction_test <- knn(train = knn_train_no_status, test = knn_test_no_status, cl=as.factor(train$Status), k = 5)
    test_time = Sys.time() - time1
    prediction_train <- knn(train = knn_train_no_status, test = knn_train_no_status, cl=as.factor(train$Status), k = 5)
    prediction_full <- knn(train = knn_train_no_status, test = knn_full_no_status, cl=as.factor(train$Status), k = 5)
    new_row <- data.frame(algorithm = "K Nearest Neighbor", time = test_time, 
                          accuracy_test = (table(test$Status,prediction_test)[1] + table(test$Status,prediction_test)[4])/nrow(test), 
                          accuracy_train = (table(train$Status,prediction_train)[1] + table(train$Status,prediction_train)[4])/nrow(train), 
                          accuracy_full = (table(phone$Status,prediction_full)[1] + table(phone$Status,prediction_full)[4])/nrow(phone), 
                          train = size)
    info_phone <- rbind(info_phone, new_row)
  }
}

####### Save the results
write.csv(info_phone,"phone_algorithm_results.csv")

####### Accuracy by training set size
mdata <- melt(info_phone, id=c("algorithm", "time", "train"))
# The number of pixels (Dont change this)
ppi <- 200
# How zoomed in the picture is
zoom <- 1.00

# This will create a .png file
png("Phone Accuracy by Training Size.png", width=10*ppi, height=4*ppi, res=ppi/zoom)
qplot(data=mdata, group=variable, y=value*100, x=train*100, colour=variable, shape=variable, facets=.~algorithm) + 
  geom_smooth(fill=NA) + theme_bw() + xlab("Training Set Size (%)") + ylab("Accuracy (%)") + 
  guides(colour =  guide_legend("Data Set Type"), shape =  guide_legend("Data Set Type")) + 
  scale_colour_discrete(labels = c("Test Data Set", "Training Data Set", "Full Data Set")) + 
  scale_shape_discrete(labels = c("Test Data Set", "Training Data Set", "Full Data Set"))
dev.off()

####### Speed by training set size
average_info_phone <- summarise(group_by(info_phone, algorithm, train),
                          time=mean(time), accuracy_test=mean(accuracy_test), accuracy_train=mean(accuracy_train), accuracy_full=mean(accuracy_full))
# This will create a .png file
png("Phone Algorithm Time.png", width=6*ppi, height=3*ppi, res=ppi/zoom)
qplot(data=average_info_phone, y=time, x=train*100, group=algorithm, colour=algorithm, geom="line" ) + 
  theme_bw() + xlab("Training Set Size (%)") + ylab("Time (seconds)") + 
  scale_colour_discrete(guide = guide_legend(title = "Algorithm")) +
  theme(legend.position = c(.20, .70), legend.background = element_rect(colour = "gray")) 
dev.off()




simple <- average_info_phone[average_info_phone$algorithm!="K Nearest Neighbor",]
simple  <- simple[simple$train>=.5,]


# The number of pixels (Dont change this)
ppi <- 200
# How zoomed in the picture is
zoom <- 1.00

png("OG paper Image speed.png", width=6*ppi, height=2*ppi, res=ppi/zoom)
qplot(data=simple , y=time, x=train*100, group=algorithm, colour=algorithm, geom="line" ) + theme_bw() + 
  scale_colour_discrete(name = "Machine Learning\nAlgorithm") + labs(x = "Training Dataset Fraction (%)", y="Time (Seconds)")
dev.off()