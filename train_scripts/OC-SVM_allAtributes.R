#dataset = read.csv('dataset/creditcard.csv')
library(caret)
library(dplyr)
library(modEvA)
library(PRROC)
library(e1071)

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

x <- subset(dataset, select = -Class) #make x variables
y <- dataset$Class #make y variable(dependent)

#experiment1
#model <- svm(x, y,type='one-classification', kernel="radial", nu =0.5, gamma= (1 / ncol(x)) )  #train an one-classification model 

#experiment2
model <- svm(x, y,type='one-classification', kernel="linear", nu =0.5)  #train an one-classification model 


print(model)
summary(model) #print summary

pred <- predict(model, subset(test, select=-Class)) #create predictions

for(j in 1:length(pred)) {
  if(pred[j]=='TRUE'){
    pred[j]=1
  }
  if(pred[j]=='FALSE'){
    pred[j]=0
  }
}

print('TRAINING END')

#predictions <- predict(model, test)
test$Class <- relevel(test$Class, ref = "0")
levels(test$Class)
y <- test$Class
result_table <- table(factor(pred), factor(y))
confMatrix <- confusionMatrix(result_table)

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
name <- paste('./ZUM/svm_oneclass/svm_roc_plot_exp2_all',  '.jpg', sep = "", collapse = NULL)
jpeg(name)
plot(roc)
dev.off()

# PR Curve and PR AUC
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
name <- paste('./ZUM/svm_oneclass/svm_pr_plot_exp2_all', '.jpg', sep = "", collapse = NULL)
jpeg(name)
plot(pr)
dev.off()

print(model)
summary(model)

