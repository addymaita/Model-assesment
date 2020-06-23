library(ggplot2)
library(dplyr)
library(caret)
library(GGally)
library(tidyr)
library(Metrics)
library(jtools)
library(lares)



#load data
train <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = TRUE)
test <- read.csv(file = "test.csv", header = TRUE,  stringsAsFactors = TRUE)
glimpse(train)

# ##names of cat variables 
# cat <- names(train)[sapply(train, class) == "factor"]
# 
# ##variables i can convert to binary 
# filter1<- Filter(function(x) nlevels(x)==2, train[,cat])
# 
# train$paved[train$Street == "Pave"] <- 1
# train$paved[train$Street != "Pave"] <- 0
# 
# train$paved[train$Utilities == "AllPub"] <- 1
# train$paved[train$Utilities != "AllPub"] <- 0



# df <-  train[ , apply(train, MARGIN = 2, function(x) sum(is.na(x)) == 0)]
df <- train %>% select(SalePrice, YearBuilt, TotalBsmtSF, Functional, Neighborhood)


set.seed(123)
# Se crean los indices de las observaciones de entrenamiento
train_df <- createDataPartition(y = df$SalePrice, p = 0.8, list = FALSE, times = 1)
datos_train <- df[train_df, ]
datos_test  <- df[-train_df, ]


testX <- subset(datos_test, select=-c(SalePrice))
testY <- datos_test$SalePrice

### model
train_control <- trainControl(method = "cv", number = 5)

# Simple linear regression model 
model_1 <- train(SalePrice ~ .,
               data = datos_train,
               method = "lm",
               trControl = train_control)

# Lasso regression model
model_2 <- train(SalePrice ~ .,
               data = datos_train,
               method = "lasso",
               trControl = train_control)

# Ridge regression model
model_3 <- train(SalePrice ~ .,
               data = datos_train,
               method = "ridge",
               trControl = train_control) # Try using "lasso"

###model comparision

modelos <- list(lm = model_1, lasso = model_2)
resultados_resamples <- resamples(modelos)

##importante en la memoria
resultados_modelos <- as.data.frame(resultados_resamples$values %>% head(10))

summary(resultados_resamples)
bwplot(resultados_resamples)
dotplot(resultados_resamples)

difs <- diff(resultados_resamples)

dotplot(difs)

densityplot(difs,
            metric = "MAE",
            auto.key = TRUE,
            pch = "|")
bwplot(difs,
       metric = "MAE")



metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head(10)

####saving model
saveRDS(model_1, file = "model_example.rda")
saveRDS(model_2, file = "model_example_2.rda")
saveRDS(model_3, file = "model_example_3.rda")

###validation 

pred_model_1 <- predict(model_1, newdata = testX,
                            type = "raw")
pred_model_2 <- predict(model_2, newdata = testX,
                        type = "raw")

pred_model_3 <- predict(model_3, newdata = testX,
                        type = "raw")
mase(testY, predicciones)

write.csv(pred_model_1, "pred_model_1.csv", row.names=FALSE)
write.csv(pred_model_2, "pred_model_2.csv",  row.names=FALSE)
write.csv(testY, "y_test.csv", row.names=FALSE)

####vizualazing 
devtools::install_github("laresbernardo/lares")

lares::mplot_lineal(tag = testY, 
                    score = pred_model_1,
                    subtitle = "House Price Regression Model",
                    model_name = "Linear regression")

lares::mplot_lineal(tag = testY, 
                    score = pred_model_2,
                    subtitle = "House Price Regression Model",
                    model_name = "Lasso")

lares::mplot_lineal(tag = testY, 
                    score = pred_model_1,
                    subtitle = "House Price Regression Model",
                    model_name = "Ridge")

##error
lares::mplot_cuts_error(tag = testY, 
                        score = pred_model_1,
                        title = "Salary Regression Model",
                        model_name = "simple_model_02")

lares::mplot_cuts_error(tag = testY, 
                        score = pred_model_2,
                        title = "Salary Regression Model",
                        model_name = "simple_model_02")

lares::mplot_cuts_error(tag = testY, 
                        score = pred_model_3,
                        title = "Salary Regression Model",
                        model_name = "simple_model_02")
##density error
lares::mplot_density(tag = log(testY), 
                     score = log(pred_model_1),
                     subtitle = "Salary Regression Model",
                     model_name = "simple_model_02")


## total
lares::mplot_full(tag = log(testY), 
                  score = log(pred_model_1),
                  splits = 10,
                  subtitle = "Salary Regression Model",
                  model_name = "simple_model_02",
                  save = T)
