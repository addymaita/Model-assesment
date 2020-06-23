require(ggplot2)
require(dplyr)
require(caret)
require(GGally)
require(tidyr)
require(Metrics)
require(jtools)
require(lares)
require(randomForest)


train <- read.csv(file = "creditcard.csv", header = TRUE, stringsAsFactors = TRUE)
glimpse(train)

table(train$Class)
##
train$Class <- as.data.frame(as.factor(as.character(train[,'Class'])))
class(train$Class)

str(train)
summary(train)

cols <- c("Class")
train[,cols] <- data.frame(apply(train[cols], 2, as.factor))
write.csv(train, "train.csv", row.names=FALSE)
train <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = TRUE)

levels(train$Class)
##Data partition 

train_df <- createDataPartition(y = train$Class, p = 0.7, list = FALSE, times = 1)
datos_train <- train[train_df, ]
datos_test  <- train[-train_df, ]


testX <- subset(datos_test, select=-c(Class))
testY <- datos_test$Class


##model train 

train_control <- trainControl(method = "cv", number = 5)

fitGrid_2 <- expand.grid(mfinal = (1:3)*3,         # This is new!
                         maxdepth = c(1, 3),       # ...and this
                         coeflearn = c("Breiman")) # ...and this

fitControl_2 <- trainControl(method = "repeatedcv", 
                             number = 2, 
                             repeats = 1)

model_1 <- train(Class ~ .,
                 data = datos_train,
                 method ="AdaBoost.M1",
                 trControl = fitControl_2,
                 tuneGrid = fitGrid_2, #and this is new, too!
                 verbose = TRUE
                )


rf <- randomForest(Class ~ ., data=datos_train, mtry=3,
                   importance=TRUE, na.action=na.omit)


saveRDS(model_1, file = "adaboost.rda")
saveRDS(rf, file = "rf.rda")

model_1_pred <- predict(rf, datos_test)
model_2_pred <- predict(model_1, datos_test)

write.csv(model_1_pred, "pred_model_1.csv", row.names=FALSE)
write.csv(model_2_pred, "pred_model_2.csv",  row.names=FALSE)
write.csv(testY, "y_test.csv", row.names=FALSE)

Metrics::recall(testY, model_1_pred)
sensitivity(testY, model_1_pred)
specificity(testY, model_1_pred)




diabetes.ci <- ci.se(diabetes_roc)
plot(diabetes.ci,type='shape')
plot(diabetes.ci,type='bars')
