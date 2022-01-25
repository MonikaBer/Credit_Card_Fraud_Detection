library("caret")
library("ellipse")


dataset <- read.csv('dataset/creditcard.csv',
                 header = TRUE, fill = TRUE, encoding = 'UTF-8')
dataset$Class <- as.factor(dataset$Class)
#dataset

tmp <- dataset[dataset$Class == 0, ]
tmp2 <- tmp[sample(nrow(tmp), 99508), ]


dataset <- rbind(dataset[dataset$Class == 1, ], tmp2)
print("\nClass distribution: ")
percentage <- prop.table(table(dataset$Class)) * 100
cbind(freq = table(dataset$Class), percentage = percentage)
#print(tmp)

#print(paste0("Data dimensions: ", dim(data)[0]))
print("\nDataset dimensions: ")
print(dim(dataset))

print("Dataset desc: ")
print(str(dataset))

print("\nAttributes types: ")
sapply(dataset, class)

head(dataset)

print(dataset$Class)

## summarize the class distribution
print("\nClass distribution: ")
percentage <- prop.table(table(dataset$Class)) * 100
cbind(freq = table(dataset$Class), percentage = percentage)


## summarize attribute distributions
print("\nDataset summary: ")
summary(dataset)


# split input and output
x <- dataset[,1:30]
y <- dataset[,31]
#print(y)


# scatterplot matrix
featurePlot(x = x, y = y, plot = "ellipse")  #too long time and too much memory


typeof(dataset$Class)


filename_1 <- sprintf("boxplot_1.png")
box_plot_1 <- featurePlot(x = x, y = y, plot = "box")
png(filename_1)
print(box_plot_1)
dev.off()

filename_2 <- sprintf("boxplot_2.png")
box_plot_2 <- featurePlot(x = x[2:30], y = y, plot = "box")
png(filename_2)
print(box_plot_2)
dev.off()

filename_3 <- sprintf("boxplot_3.png")
box_plot_3 <- featurePlot(x = x[2:29], y = y, plot = "box")
png(filename_3)
print(box_plot_3)
dev.off()

# density plots for each attribute by class value
filename_2 <- sprintf("density_plot.png")
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
density_plot_2 <- featurePlot(x = x, y = y, plot = "density", scales = scales)
png(filename_2)
print(density_plot_2)
