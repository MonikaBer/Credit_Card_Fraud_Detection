library(caret)
library(dplyr)
library(modEvA)
library(PRROC)

dataset = read.csv('dataset/creditcard.csv')
dataset <- dataset[, c(1:31)]                      #all attributes
# dataset

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

#normalization
dataset[-31] = scale(dataset[-31])    #for all attributes
test[-31] = scale(test[-31])          #for all attributes
# head(dataset)
# head(test)

#cost (C) set
grid_set <- expand.grid(C = 10)    #for all attributes

#5-fold CV
control <- trainControl(method = "cv", number = 5)

modelsList <- list()

set.seed(1000)  #set const seed
# train model with class.weights and cost
fit <- train(Class ~ .,
            data = dataset,
            method = "svmLinear",
            class.weights = c("1" = 5, "0" = 995),
	        metric = 'Sens',
	        tuneGrid = grid_set,
            trControl = control,
            tuneLength = 1)

print(fit)
model <- fit
predictions <- predict(model, test)
confMatrix <- confusionMatrix(predictions, test$Class)

print('Confusion matrix: ')
print(confMatrix)

print('Recall:')
rec = recall(predictions, test$Class)
print(rec)

print('Precision:')
prec = precision(predictions, test$Class)
print(prec)

print('F1:')
f1 = (2 * prec * rec) / (prec + rec)
print(f1)

fg <- predictions[test$Class == 1]
bg <- predictions[test$Class == 0]

i = 1
# ROC Curve and ROC AUC
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
name <- paste('roc_plot_', toString(i), '.jpg', sep = "", collapse = NULL)
jpeg(name)
plot(roc)
dev.off()

# PR Curve and PR AUC
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
name <- paste('pr_plot_', toString(i), '.jpg', sep = "", collapse = NULL)
jpeg(name)
plot(pr)
dev.off()

#plot(model)
print(model)
summary(model)
