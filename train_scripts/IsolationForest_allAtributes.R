#dataset = read.csv('dataset/creditcard.csv')
library(caret)
library(isotree)
library(dplyr)
library(modEvA)
library(PRROC)

dataset_read = read.csv('creditcard.csv')

dataset <- as.data.frame(scale(dataset_read[1:30]))
dataset$Class <- dataset_read$Class

# resampling
d0 <- dataset[dataset$Class == 0,]
d1 <- dataset[dataset$Class == 1,]
d0$Class = 1
d1$Class = 0
dataset <- rbind(d0, d1)

#geting smaller set: 100 tys. (99508:492)
tmp <- dataset[dataset$Class == 1, ]
tmp_correct <- tmp[sample(nrow(tmp), 99508), ] # te poprawne
tmp_anomaly <- dataset[dataset$Class == 0, ]  # te z anomaliami

tmp_correct$Class <- as.factor(tmp_correct$Class)
tmp_anomaly$Class <- as.factor(tmp_anomaly$Class)

# split dataset to training and test sets (80 tys.:20 tys. => 4:1)
# test set: 492 with anomaly + 19508 correct
# train set: 80 000 correct

# split temp_correct dataset to training and test sets (80 tys.:19508 => 4:1)
test_index <- createDataPartition(tmp_correct$Class, p = 0.80395546086, list = FALSE)

test <- rbind(tmp_correct[-test_index,], tmp_anomaly)
test$Class <- as.factor(test$Class)
dataset <- tmp_correct[test_index,]


# Isolation Forest Algorithm
# X and Y - datasets without classes to constructing Isolation Forest
X<-dataset[, c(1:30)]
Y<-test[, c(1:30)]

modelsList <- list()

i = 100
for (ntree in seq(from = 100, to = 1000, by = 100) ) {
  print("Constructing model:")
  print(ntree)
  set.seed(100)
  data_size = NROW(X)
  ### Fit a small isolation forest model
  
  # experiment 1
  model <- isolation.forest(X, ntrees = ntree,
                          nthreads = 1,
                          sample_size = data_size,
                          max_depth = ceiling(log2(sample_size))
                          )
  
  #experiment 2
  #model <- isolation.forest(X, ntrees = ntree,
  #                          nthreads = 1,
  #                          sample_size = NROW(X),
  #                          max_depth = sample_size
  #)
  
  #experiment3
  #model <- isolation.forest(X, ntrees = ntree,
  #                          nthreads = 1,
  #                          sample_size = ceiling(NROW(X) /2),
  #                          max_depth = ceiling(log2(sample_size))
  #)
  
  
  key <- toString(ntree)
  modelsList[[key]] <- model
  
  a <- paste('model ', toString(i), ':', sep = "", collapse = NULL)
  print(a)
  
  ### Prediction of outlier score
  pred_Y <- predict(model, Y)
  
  #change prediction probabilities to 0-1 values
  pred_Y_round <- pred_Y
  
  for(j in 1:length(pred_Y_round)) {
    pred_Y_round[j]=as.integer(1 - round(pred_Y_round[j]) )
  }
  
  predictions <- pred_Y_round
  test$Class <- relevel(test$Class, ref = "0")
  levels(test$Class)
  y <- test$Class
  result_table <- table(factor(predictions), factor(y))
  confMatrix <- confusionMatrix(result_table, positive = '0')
  
  print('Confusion matrix: ')
  print(confMatrix)
  
  print('Recall:')
  rec = recall(result_table)
  print(rec)
  
  print('Precision:')
  prec = precision(result_table)
  print(prec)
  
  print('F1:')
  f1 = (2 * prec * rec) / (prec + rec)
  print(f1)
  
  fg <- predictions[test$Class == 1]
  bg <- predictions[test$Class == 0]
  
  # ROC Curve and ROC AUC
  roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  name <- paste('./ZUM/wykresy_all/roc_plot_', toString(i),'_experiment1', '.jpg', sep = "", collapse = NULL)
  jpeg(name)
  plot(roc)
  dev.off()
  
  # PR Curve and PR AUC
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  name <- paste('./ZUM/wykresy_all/pr_plot_', toString(i),'_experiment', '.jpg', sep = "", collapse = NULL)
  jpeg(name)
  plot(pr)
  dev.off()
  
  print(model)
  summary(model)
  
  i <- i+100
}

#Compare results
results <- resamples(modelsList)
summary(results)

print("END")
