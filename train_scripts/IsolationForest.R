#install.packages("caret")
#install.packages("ellipse")
library("caret")
library("ellipse")


dataset <- read.csv('dataset/creditcard.csv',
                 header = TRUE, fill = TRUE, encoding = 'UTF-8')
dataset$Class <- as.factor(dataset$Class)

#attrs <- c(11,12,13,15,17,18,31)
x <- dataset[, c(11,12,13,15,17,18)]
y <- dataset[, 31]
