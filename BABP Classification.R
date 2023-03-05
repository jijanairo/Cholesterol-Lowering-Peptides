setwd("~/Documents/Research/Bile acid")

set.seed(101831)

library(Peptides)
library(dplyr)
library(caret)
library(ggpubr)
library(iml)
library(pROC)

set.seed(101831)
df <- read.csv("BA.csv", header = TRUE) 
out <- vhseScales(seq = df$Sequence)

output <-as.data.frame(do.call(rbind, out))
mother_dataset <- cbind(df, output)

mother_dataset$Class <- as.factor(mother_dataset$Class)

str(mother_dataset)

dataset <- mother_dataset[3:11]


validation_index <- createDataPartition(dataset$Class, p=0.7, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]
validation$Class <- as.factor(validation$Class)

control <- trainControl(method="cv", number=10, classProbs = T, savePredictions = "all", summaryFunction = twoClassSummary)
metric <- "ROC"

fit.svmR <- train(Class~ ., data=dataset, method="svmRadial", metric=metric, trControl=control)
fit.svmR

fit.svmR1 <- train(Class~ VHSE1 + VHSE2 + VHSE3 + VHSE4 + VHSE5 + VHSE6 + VHSE7, data=dataset, method="svmRadial", metric=metric, trControl=control)
fit.svmR1

fit.svmR2 <- train(Class~ VHSE1 + VHSE2 + VHSE3 + VHSE4 + VHSE5 + VHSE7 + VHSE8, data=dataset, method="svmRadial", metric=metric, trControl=control)
fit.svmR2

fit.svmR3 <- train(Class~ VHSE1 + VHSE2 + VHSE3 + VHSE4 + VHSE5 + VHSE7, data=dataset, method="svmRadial", metric=metric, trControl=control)
fit.svmR3

# predicting using best model
validation$Class <- as.factor(validation$Class)
predictions <- predict(fit.svmR3, validation)
show <- confusionMatrix(predictions, validation$Class, mode = "everything")
show

#External validation
df1 <- read.csv("external validation 2.csv", header = TRUE) 
out1 <- vhseScales(seq = df1$Sequence)

output1 <-as.data.frame(do.call(rbind, out1))
mother_dataset1 <- cbind(df1, output1)

mother_dataset1$Class <- as.factor(mother_dataset1$Class)

mother_dataset1

str(mother_dataset1)

external_validation <- mother_dataset1[2:10]


predictions_valid <- predict(fit.svmR3, external_validation)
show1 <- confusionMatrix(predictions_valid, external_validation$Class, mode = "everything")
show1

ggplot(df, aes(x = Feature, y = Score, fill = Method)) + geom_bar(stat = "identity", color = "black", position = "dodge") + theme_clean() + geom_vline(xintercept = 4.5, linetype = "dashed", color = "red", linewidth = 1) + geom_vline(xintercept = 5.5, linetype = "dashed", color = "red", linewidth = 1) + scale_fill_manual(values=c('#999999','#E69F00'))