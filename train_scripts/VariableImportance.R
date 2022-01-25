library(caret)
library(isotree)
library(dplyr)
library(modEvA)
library(PRROC)

library(randomForest)

dataset_read = read.csv('creditcard.csv')

dataset <- dataset_read

#normalisation
dataset <- as.data.frame(scale(dataset_read[1:30]))
dataset$Class <- dataset_read$Class

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

# split dataset to training and test sets (80 tys.:20 tys. => 4:1)
test_index <- createDataPartition(dataset$Class, p = 0.80, list = FALSE)
test <- dataset[-test_index,]
dataset <- dataset[test_index,]

modelsList <- list()

i = 100
for (ntree in seq(from = 100, to = 100, by = 500) ) {
  print("Constructing model:")
  print(ntree)
  set.seed(100)

  model <- randomForest(Class ~ . , data = dataset,ntree = ntree, importance = TRUE)
  importance(model)
  varImpPlot(model)
  
  key <- toString(ntree)
  modelsList[[key]] <- model
  
  a <- paste('model ', toString(i), ':', sep = "", collapse = NULL)
  print(a)
  
  name <- paste('./ZUM/VariableImportance/VariableImportance_normalised_', toString(i), '.jpg', sep = "", collapse = NULL)
  jpeg(name)
  varImpPlot(model)
  dev.off()
  
  i <- i+100
}


print("END")
