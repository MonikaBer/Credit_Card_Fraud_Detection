library(randomForest)
library(caret)
library(dplyr)
library(modEvA)
# library(MLmetrics)


dataset = read.csv('dataset/creditcard.csv')
# dataset <- dataset[, c(11,12,13,15,17,18,31)]
dataset <- dataset[, c(11,12,31)]
#dataset

# resampling
d0 <- dataset[dataset$Class == 0,]
d1 <- dataset[dataset$Class == 1,]
d0$Class = 1
d1$Class = 0
dataset <- rbind(d0, d1)

tmp <- dataset[dataset$Class == 1, ]
tmp2 <- tmp[sample(nrow(tmp), 99508), ]
dataset <- rbind(dataset[dataset$Class == 0, ], tmp2)

dataset$Class <- as.factor(dataset$Class)

# split dataset to training and test sets (80:1)
test_index <- createDataPartition(dataset$Class, p = 0.80, list = FALSE)
test <- dataset[-test_index,]
dataset <- dataset[test_index,]

control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

set.seed(7)
fit.rf <- train(Class ~ ., data = dataset, method = "rf", metric = metric, trControl = control)
# print(fit.rf)
# summary(fit.rf)

predictions <- predict(fit.rf, test)
confMatrix <- confusionMatrix(predictions, testn$Class)

print('Confusion matrix: ')
confMatrix

print('Recall:')
rec = recall(predictions, test$Class)
rec

print('Precision:')
prec = precision(predictions, test$Class)
prec

print('F1:')
f1 = (2 * prec * rec) / (prec + rec)
f1

print('PRAUC:')
AUC(obs = as.integer(validation$Class), pred = as.integer(predictions), curve = 'PR', simplif = TRUE)
