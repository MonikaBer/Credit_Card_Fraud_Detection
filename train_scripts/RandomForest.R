library(randomForest)
library(caret)
library(dplyr)
library(modEvA)
library(PRROC)

dataset = read.csv('dataset/creditcard.csv')
dataset <- dataset[, c(10,11,12,14,16,17,31)]       #selected attributes
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

dataset[-7] = scale(dataset[-7])    #for selected attributes
test[-7] = scale(test[-7])          #for selected attributes
# head(dataset)
# head(test)


grid_set <- expand.grid(.mtry = 2)    #for selected attributes  (mtry set)

#5-fold CV
control <- trainControl(method = "cv", number = 5, search = 'grid',
                        savePredictions = TRUE,
                        summaryFunction = twoClassSummary)

modelsList <- list()

#ntree set
for (ntree in c(1, 50, 100, 200, 500, 1000)) {
    set.seed(1000)    #const seed
    #train model with ntree and sampsize
    fit <- train(Class ~ .,
                data = dataset,
                method = "rf",
                ntree = ntree,
                strata = dataset$Class,
                sampsize = rep(sum(dataset$Class == 0), 2),
                metric = 'Sens',
                trControl = control,
                tuneGrid = grid_set)

    key <- toString(ntree)
    modelsList[[key]] <- fit
}

print('TRAINING END')

#Compare results
results <- resamples(modelsList)
summary(results)

print('Check models')

i = 0
for (model in modelsList) {
    i <- i + 1
    a <- paste('model ', toString(i), ':', sep = "", collapse = NULL)
    print(a)

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
}
